{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Driver.OpenAI
  ( openAiDriver
  , openAiDynamicDriver
  , parseSseChunk
  , splitThinkingChunks
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_, when, unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (minimumBy)
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
openAiDriver cfg = openAiDynamicDriver cfg (pure (modelName cfg))

-- | OpenAI driver supporting dynamic model resolution per request
openAiDynamicDriver :: Config -> IO Text -> IO ModelDriver
openAiDynamicDriver Config{..} getActiveModel = do
  manager <- newTlsManager
  pure ModelDriver
    { streamCompletion = \turns tools callback -> do
        if T.null apiKey
          then do
            callback (ChunkText "\n[Configuration Warning: API key is empty. Set LAMBDA_API_KEY or OPENROUTER_API_KEY in your environment, or configure .lambda/config.json]\n")
            callback ChunkDone
          else do
            curModel <- getActiveModel
            let endpoint = T.unpack apiBaseUrl <> "/chat/completions"
                messages = turnsToOpenAIPayload turns
                baseBody =
                  [ "model"    .= curModel
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
processSseStream reader callback = do
  inThinkingRef <- newIORef False
  tagBufRef <- newIORef ("" :: Text)

  let emitChunk c = callback c

      processTextContent raw = do
        prevBuf <- readIORef tagBufRef
        let combined = prevBuf <> raw
            (toProcess, newBuf) = splitPotentialTag combined
        writeIORef tagBufRef newBuf
        unless (T.null toProcess) $ do
          inTh <- readIORef inThinkingRef
          let (nextInTh, chunks) = splitThinkingChunks inTh toProcess
          writeIORef inThinkingRef nextInTh
          mapM_ emitChunk chunks

      splitPotentialTag s =
        let suffixes = [ T.drop i s | i <- [max 0 (T.length s - 10) .. T.length s - 1] ]
            isPrefixOfTag suf = any (suf `T.isPrefixOf`)
              [ "<think>", "<thought>", "</think>", "</thought>" ]
        in case filter isPrefixOfTag suffixes of
             (longest : _) | longest /= "<think>" && longest /= "<thought>"
                          && longest /= "</think>" && longest /= "</thought>" ->
               (T.dropEnd (T.length longest) s, longest)
             _ -> (s, "")

      handleLine line =
        let trimmed = T.strip line
        in case parseSseItems trimmed of
             Left isDone -> when isDone $ do
               buf <- readIORef tagBufRef
               unless (T.null buf) $ do
                 inTh <- readIORef inThinkingRef
                 let (_, chunks) = splitThinkingChunks inTh buf
                 mapM_ emitChunk chunks
               emitChunk ChunkDone
             Right items ->
               forM_ items $ \case
                 Left chunk  -> emitChunk chunk
                 Right contentTxt -> processTextContent contentTxt

      loop acc = do
        bs <- brRead reader
        if BS.null bs
          then do
            buf <- readIORef tagBufRef
            unless (T.null buf) $ do
              inTh <- readIORef inThinkingRef
              let (_, chunks) = splitThinkingChunks inTh buf
              mapM_ emitChunk chunks
            emitChunk ChunkDone
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

  loop ("" :: Text)

-- | Intermediate representation of SSE deltas
parseSseItems :: Text -> Either Bool [Either StreamChunk Text]
parseSseItems rawLine
  | "data: [DONE]" `T.isInfixOf` rawLine = Left True
  | "data: " `T.isPrefixOf` rawLine =
      let payload = T.drop 6 rawLine
      in case Aeson.decode (BL.fromStrict $ TE.encodeUtf8 payload) of
           Just (Aeson.Object obj) -> Right (extractDeltas obj)
           _                       -> Right []
  | otherwise = Right []

-- | Pure function for single-chunk testing (delegates with initial inThinking = False)
parseSseChunk :: Text -> [StreamChunk]
parseSseChunk rawLine =
  case parseSseItems rawLine of
    Left True   -> [ChunkDone]
    Left False  -> []
    Right items -> concatMap toChunk items
  where
    toChunk (Left c)  = [c]
    toChunk (Right t) = snd (splitThinkingChunks False t)

extractDeltas :: Aeson.Object -> [Either StreamChunk Text]
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
          -- Priority 1: reasoning_content or reasoning (DeepSeek, OpenRouter, Qwen, etc.)
          mReasoning1 <- delta .:? "reasoning_content"
          mReasoning2 <- delta .:? "reasoning"
          let mReasoning = case mReasoning1 of
                Just r | not (T.null r) -> Just r
                _ -> case mReasoning2 of
                  Just r | not (T.null r) -> Just r
                  _ -> Nothing
          case mReasoning of
            Just r -> pure [Left (ChunkThinking r)]
            Nothing -> do
              -- Priority 2: tool_calls delta (support multi-tool-call arrays with index tracking)
              mToolCalls <- delta .:? "tool_calls"
              case mToolCalls of
                Just (tcList :: [Aeson.Value]) | not (null tcList) -> do
                  chunkLists <- forM tcList $ \case
                    Aeson.Object tc -> do
                      mId <- tc .:? "id"
                      mFn <- tc .:? "function"
                      mIdx <- tc .:? "index"
                      (mName, mArgs) <- case mFn of
                        Just (Aeson.Object fn) -> do
                          n <- fn .:? "name"
                          a <- fn .:? "arguments"
                          pure (n, a)
                        _ -> pure (Nothing, Nothing)
                      let startChunk = case (mId, mName) of
                            (Just cid, Just name) -> [Left (ChunkToolCallStart cid name)]
                            _                     -> []
                          cidRef = case mId of
                            Just cid -> cid
                            Nothing  -> case (mIdx :: Maybe Int) of
                              Just idx -> "idx_" <> T.pack (show idx)
                              Nothing  -> ""
                          argChunk = case mArgs of
                            Just args | not (T.null args) -> [Left (ChunkToolCallArgs cidRef args)]
                            _                             -> []
                      pure (startChunk ++ argChunk)
                    _ -> pure []
                  pure (concat chunkLists)
                _ -> do
                  -- Priority 3: content delta
                  mContent <- delta .:? "content"
                  case mContent of
                    Just txt | not (T.null txt) -> pure [Right txt]
                    _ -> pure []
        _ -> pure []

-- | Pure stateful splitter of inline reasoning tags (<think>...</think>, <thought>...</thought>)
splitThinkingChunks :: Bool -> Text -> (Bool, [StreamChunk])
splitThinkingChunks inThinking txt
  | T.null txt = (inThinking, [])
  | not inThinking =
      case findEarliestTag openTags txt of
        Just (tag, idx) ->
          let before = T.take idx txt
              rest = T.drop (idx + T.length tag) txt
              chunksBefore = [ChunkText before | not (T.null before)]
              (nextState, remainingChunks) = splitThinkingChunks True rest
          in (nextState, chunksBefore ++ remainingChunks)
        Nothing ->
          (False, [ChunkText txt])
  | otherwise = -- inThinking is True
      case findEarliestTag closeTags txt of
        Just (tag, idx) ->
          let inside = T.take idx txt
              rest = T.drop (idx + T.length tag) txt
              chunksInside = [ChunkThinking inside | not (T.null inside)]
              (nextState, remainingChunks) = splitThinkingChunks False rest
          in (nextState, chunksInside ++ remainingChunks)
        Nothing ->
          (True, [ChunkThinking txt])
  where
    openTags = ["<think>", "<thought>"]
    closeTags = ["</think>", "</thought>"]

    findEarliestTag tags s =
      let occurrences = [ (tag, idx)
                        | tag <- tags
                        , let (b, m) = T.breakOn tag s
                        , not (T.null m)
                        , let idx = T.length b
                        ]
      in case occurrences of
           [] -> Nothing
           xs -> Just $ minimumBy (\(_, a) (_, b) -> compare a b) xs
