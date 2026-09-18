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
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.IO (BufferMode(..), Handle, hFlush, hSetBuffering)
import System.Process.Typed

import Lambda.Types (Framing(..))

data JsonRpcClient = JsonRpcClient
  { rpcProcess      :: !(Process Handle Handle Handle)
  , rpcReqId        :: !(TVar Int)
  , rpcPending      :: !(TVar (Map Int (TMVar (Either Text Aeson.Value))))
  , rpcReader       :: !(Async ())
  , rpcStderrReader :: !(Async ())
  , _rpcFraming     :: !Framing
  , rpcWriteLock    :: !(MVar ())
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
  writeLock <- newMVar ()

  let readerAction = readLineFramedLoop (getStdout p) pendingVar
      drainStderrAction = drainStderrLoop (getStderr p)

  reader <- async readerAction
  errReader <- async drainStderrAction
  pure $ JsonRpcClient p reqIdVar pendingVar reader errReader framing writeLock

-- | Continuously drains stderr to prevent 64KB OS pipe buffer exhaustion from deadlocking the process
drainStderrLoop :: Handle -> IO ()
drainStderrLoop hErr = do
  res <- try (BS.hGetSome hErr 4096)
  case res of
    Left (_ :: SomeException) -> pure ()
    Right bs | BS.null bs     -> pure ()
             | otherwise      -> drainStderrLoop hErr

-- | Newline-delimited JSON reader with robust EOF / exception recovery
readLineFramedLoop :: Handle -> TVar (Map Int (TMVar (Either Text Aeson.Value))) -> IO ()
readLineFramedLoop hOut pendingVar = do
  res <- try (BS.hGetLine hOut)
  case res of
    Left (_ :: SomeException) -> atomically $ do
      pend <- readTVar pendingVar
      mapM_ (\tmvar -> putTMVar tmvar (Left "MCP client disconnected: process exited or pipe closed")) (Map.elems pend)
      writeTVar pendingVar Map.empty
    Right line -> do
      handleIncomingJson (BL.fromStrict line) pendingVar
      readLineFramedLoop hOut pendingVar

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
      wireBytes = encoded <> "\n"

  writeResult <- withMVar (rpcWriteLock client) $ \() -> do
    try (BL.hPut (getStdin $ rpcProcess client) wireBytes >> hFlush (getStdin $ rpcProcess client))

  case writeResult of
    Left (_ :: SomeException) -> do
      atomically $ modifyTVar' (rpcPending client) (Map.delete reqId)
      pure $ Left "MCP client disconnected: process exited or pipe closed"
    Right () ->
      atomically $ takeTMVar replyVar

sendNotification :: JsonRpcClient -> Text -> Aeson.Value -> IO ()
sendNotification client method params = do
  let payload = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method"  .= method
        , "params"  .= params
        ]
      encoded = Aeson.encode payload
      wireBytes = encoded <> "\n"

  withMVar (rpcWriteLock client) $ \() -> do
    _ <- try (BL.hPut (getStdin $ rpcProcess client) wireBytes >> hFlush (getStdin $ rpcProcess client)) :: IO (Either SomeException ())
    pure ()

stopRpcClient :: JsonRpcClient -> IO ()
stopRpcClient client = do
  cancel (rpcReader client)
  cancel (rpcStderrReader client)
  _ <- (try (stopProcess (rpcProcess client)) :: IO (Either SomeException ()))
  pure ()

