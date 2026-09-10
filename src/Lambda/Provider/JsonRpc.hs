{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Provider.JsonRpc
  ( JsonRpcClient
  , startRpcClient
  , startRpcClientWithEnv
  , stopRpcClient
  , sendRequest
  , sendNotification
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.IO (BufferMode(..), Handle, hSetBuffering)
import System.Process.Typed
import Text.Read (readMaybe)

import Lambda.Types (Framing(..))

data JsonRpcClient = JsonRpcClient
  { rpcProcess  :: !(Process Handle Handle Handle)
  , rpcReqId    :: !(TVar Int)
  , rpcPending  :: !(TVar (Map Int (TMVar (Either Text Aeson.Value))))
  , rpcReader   :: !(Async ())
  , rpcFraming  :: !Framing
  }

startRpcClient :: Framing -> FilePath -> [Text] -> IO JsonRpcClient
startRpcClient framing cmd args = startRpcClientWithEnv framing cmd args []

startRpcClientWithEnv :: Framing -> FilePath -> [Text] -> [(String, String)] -> IO JsonRpcClient
startRpcClientWithEnv framing cmd args extraEnv = do
  currEnv <- getEnvironment
  let mergedEnv = if null extraEnv then currEnv else extraEnv ++ currEnv
      baseProc = setStdin createPipe
               $ setStdout createPipe
               $ setStderr createPipe
               $ proc cmd (map T.unpack args)
      pConf = if null extraEnv then baseProc else setEnv mergedEnv baseProc
  p <- startProcess pConf
  hSetBuffering (getStdin p) LineBuffering
  hSetBuffering (getStdout p) LineBuffering

  reqIdVar <- newTVarIO 1
  pendingVar <- newTVarIO Map.empty

  let readerAction = case framing of
        LineFramed   -> readLineFramedLoop (getStdout p) pendingVar
        HeaderFramed -> readHeaderFramedLoop (getStdout p) pendingVar

  reader <- async readerAction
  pure $ JsonRpcClient p reqIdVar pendingVar reader framing

-- | Newline-delimited JSON reader (standard MCP stdio)
readLineFramedLoop :: Handle -> TVar (Map Int (TMVar (Either Text Aeson.Value))) -> IO ()
readLineFramedLoop hOut pendingVar = forever $ do
  line <- BL.fromStrict <$> BS.hGetLine hOut
  handleIncomingJson line pendingVar

-- | MIME header-framed JSON reader (standard Clangd / LSP)
readHeaderFramedLoop :: Handle -> TVar (Map Int (TMVar (Either Text Aeson.Value))) -> IO ()
readHeaderFramedLoop hOut pendingVar = forever $ do
  len <- readContentLengthHeader hOut
  if len > 0
    then do
      body <- BL.hGet hOut len
      handleIncomingJson body pendingVar
    else pure ()

readContentLengthHeader :: Handle -> IO Int
readContentLengthHeader h = do
  line <- BSC.hGetLine h
  if BSC.null (BSC.filter (/= '\r') line)
    then pure 0
    else if "Content-Length:" `BSC.isPrefixOf` line
      then do
        let lenStr = BSC.unpack $ BSC.strip $ BSC.drop 15 line
            len = maybe 0 id (readMaybe lenStr)
        -- Read following empty line separator
        _ <- BSC.hGetLine h
        pure len
      else readContentLengthHeader h

handleIncomingJson :: BL.ByteString -> TVar (Map Int (TMVar (Either Text Aeson.Value))) -> IO ()
handleIncomingJson rawJson pendingVar = do
  case Aeson.decode rawJson of
    Just (Aeson.Object obj) -> do
      case (parseEither (.: "id") obj, parseEither (.: "result") obj) of
        (Right reqId, Right res) -> atomically $ do
          pend <- readTVar pendingVar
          case Map.lookup reqId pend of
            Just tmvar -> do
              putTMVar tmvar (Right res)
              writeTVar pendingVar (Map.delete reqId pend)
            Nothing -> pure ()
        _ -> case (parseEither (.: "id") obj, parseEither (.: "error") obj) of
          (Right reqId, Right (Aeson.Object errObj)) -> do
            let msg = case parseEither (.: "message") errObj of
                  Right m -> m
                  _       -> "RPC error"
            atomically $ do
              pend <- readTVar pendingVar
              case Map.lookup reqId pend of
                Just tmvar -> do
                  putTMVar tmvar (Left msg)
                  writeTVar pendingVar (Map.delete reqId pend)
                Nothing -> pure ()
          _ -> pure ()
    _ -> pure ()

sendRequest :: JsonRpcClient -> Text -> Aeson.Value -> IO (Either Text Aeson.Value)
sendRequest client method params = do
  (reqId, replyVar) <- atomically $ do
    currId <- readTVar (rpcReqId client)
    writeTVar (rpcReqId client) (currId + 1)
    tmvar <- newEmptyTMVar
    modifyTVar' (rpcPending client) (Map.insert currId tmvar)
    pure (currId, tmvar)

  let payload = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id"      .= reqId
        , "method"  .= method
        , "params"  .= params
        ]
      encoded = Aeson.encode payload
      wireBytes = case rpcFraming client of
        LineFramed -> encoded <> "\n"
        HeaderFramed ->
          let len = BL.length encoded
          in "Content-Length: " <> BL.fromStrict (BSC.pack (show len)) <> "\r\n\r\n" <> encoded

  BL.hPut (getStdin $ rpcProcess client) wireBytes
  atomically $ takeTMVar replyVar

sendNotification :: JsonRpcClient -> Text -> Aeson.Value -> IO ()
sendNotification client method params = do
  let payload = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method"  .= method
        , "params"  .= params
        ]
      encoded = Aeson.encode payload
      wireBytes = case rpcFraming client of
        LineFramed -> encoded <> "\n"
        HeaderFramed ->
          let len = BL.length encoded
          in "Content-Length: " <> BL.fromStrict (BSC.pack (show len)) <> "\r\n\r\n" <> encoded

  BL.hPut (getStdin $ rpcProcess client) wireBytes

stopRpcClient :: JsonRpcClient -> IO ()
stopRpcClient client = do
  cancel (rpcReader client)
  stopProcess (rpcProcess client)
