{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Driver.OpenAI
  ( openAiDriver
  , parseSseChunk
  ) where

import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hContentType, hAuthorization)
import Network.HTTP.Types.Status (statusCode, statusMessage)

import Lambda.Config (Config(..))
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Engine.Compactor (turnsToOpenAIPayload)
import Lambda.Types

openAiDriver :: Config -> IO ModelDriver
openAiDriver Config{..} = do
  manager <- newTlsManager
  pure ModelDriver
    { streamCompletion = \turns tools callback -> do
        if T.null apiKey
          then do
            callback (ChunkText "\n[Configuration Warning: API key is empty. Set LAMBDA_API_KEY or OPENROUTER_API_KEY in your environment, or configure .lambda/config.json]\n")
            callback ChunkDone
          else do
            let endpoint = T.unpack apiBaseUrl <> "/chat/completions"
                messages = turnsToOpenAIPayload turns
                baseBody =
                  [ "model"    .= modelName
                  , "messages" .= messages
                  , "stream"   .= True
                  ]
                reqPayload = if null tools
                  then Aeson.object baseBody
                  else Aeson.object (baseBody ++ ["tools" .= tools])

            res <- try $ do
              initReq <- parseRequest endpoint
              let customHdrs = [ (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- Map.toList customHeaders ]
                  allHdrs =
                    [ (hContentType, "application/json")
                    , (hAuthorization, "Bearer " <> TE.encodeUtf8 apiKey)
                    ] ++ customHdrs
                  req = initReq
                    { method = "POST"
                    , requestHeaders = allHdrs
                    , requestBody = RequestBodyLBS (Aeson.encode reqPayload)
                    , checkResponse = \_ _ -> pure ()
                    }

              withResponse req manager $ \response -> do
                let status = responseStatus response
                    code = statusCode status
                if code < 200 || code >= 300
                  then do
                    errChunks <- brConsume (responseBody response)
                    let errBody = TE.decodeUtf8Lenient (BS.concat errChunks)
                    callback (ChunkText $ "\n[API Error " <> T.pack (show code) <> " " <> TE.decodeUtf8Lenient (statusMessage status) <> ": " <> errBody <> "]\n")
                    callback ChunkDone
                  else do
                    let reader = responseBody response
                    processSseStream reader callback

            case res of
              Left (ex :: SomeException) -> do
                callback (ChunkText $ "\n[HTTP/Network Exception: " <> T.pack (show ex) <> "]\n")
                callback ChunkDone
              Right () -> pure ()
    }

processSseStream :: BodyReader -> (StreamChunk -> IO ()) -> IO ()
processSseStream reader callback = loop ("" :: Text)
  where
    loop acc = do
      bs <- brRead reader
      if BS.null bs
        then callback ChunkDone
        else do
          let chunk = acc <> TE.decodeUtf8Lenient bs
              (lines', rest) = splitLines chunk
          mapM_ handleLine lines'
          loop rest

    splitLines txt =
      let ls = T.splitOn "\n" txt
      in if null ls
           then ([], "")
           else (init ls, last ls)

    handleLine line =
      let trimmed = T.strip line
      in mapM_ callback (parseSseChunk trimmed)

-- | Parses a single SSE data line from an OpenAI-compatible stream
parseSseChunk :: Text -> [StreamChunk]
parseSseChunk rawLine
  | "data: [DONE]" `T.isInfixOf` rawLine = [ChunkDone]
  | "data: " `T.isPrefixOf` rawLine =
      let payload = T.drop 6 rawLine
      in case Aeson.decode (BL.fromStrict $ TE.encodeUtf8 payload) of
           Just (Aeson.Object obj) -> extractDeltas obj
           _                       -> []
  | otherwise = []

extractDeltas :: Aeson.Object -> [StreamChunk]
extractDeltas obj =
  case parseEither parseChoices (Aeson.Object obj) of
    Right chunks -> chunks
    _            -> []
  where
    parseChoices = Aeson.withObject "Response" $ \o -> do
      choices <- o .: "choices"
      case choices of
        (Aeson.Object c : _) -> do
          delta <- c .: "delta"
          -- Priority 1: reasoning_content (openrouter/free, Qwen reasoning, OpenRouter)
          mReasoning <- delta .:? "reasoning_content"
          case mReasoning of
            Just r | not (T.null r) -> pure [ChunkThinking r]
            _ -> do
              -- Priority 2: tool_calls delta
              mToolCalls <- delta .:? "tool_calls"
              case mToolCalls of
                Just (Aeson.Object tc : _) -> do
                  mId <- tc .:? "id"
                  mFn <- tc .:? "function"
                  (mName, mArgs) <- case mFn of
                    Just (Aeson.Object fn) -> do
                      n <- fn .:? "name"
                      a <- fn .:? "arguments"
                      pure (n, a)
                    _ -> pure (Nothing, Nothing)
                  let startChunk = case (mId, mName) of
                        (Just cid, Just name) -> [ChunkToolCallStart cid name]
                        _                     -> []
                      cidRef = maybe "" id mId
                      argChunk = case mArgs of
                        Just args | not (T.null args) -> [ChunkToolCallArgs cidRef args]
                        _                             -> []
                  pure (startChunk ++ argChunk)
                _ -> do
                  -- Priority 3: content delta (with inline <think> tag support)
                  mContent <- delta .:? "content"
                  case mContent of
                    Just txt | not (T.null txt) ->
                      if "<think>" `T.isInfixOf` txt || "</think>" `T.isInfixOf` txt
                        then pure [ChunkThinking txt]
                        else pure [ChunkText txt]
                    _ -> pure []
        _ -> pure []
