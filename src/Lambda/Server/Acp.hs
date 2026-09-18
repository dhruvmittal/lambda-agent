{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lambda.Server.Acp
  ( runAcpServer
  , handleAcpRequest
  , handleAcpResponse
  , callClientRpc
  , AcpServerState(..)
  , initAcpServerState
  , initAcpServerStateWithSink
  , startAcpSession
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, try)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (doesDirectoryExist, removeFile)
import System.FilePath ((</>), (<.>))
import System.IO
  ( stdin
  , stdout
  , stderr
  , hFlush
  , hSetBuffering
  , hSetEncoding
  , utf8
  , BufferMode(..)
  , isEOF
  )
import System.Timeout (timeout)

import Lambda.Config (Config(..), loadConfig)
import Lambda.Core.EngineInterface
  ( initEngineChannels
  , startEngineLoop
  , EngineChannels(..)
  )
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider (registerTools, registerTool, emptyRegistry)
import Lambda.Driver.OpenAI (openAiDynamicDriver)
import Lambda.Engine.Security (initSecurityWithRoot, resolvePrompt)
import Lambda.Engine.Session (loadSession, listMeaningfulSessions, Session(..), SessionMeta(..))
import Lambda.Engine.State (initEngineStateWithSession, AppEngineState(..))
import Lambda.Engine.SubAgent (spawnSpecialistSubAgentTool)
import Lambda.Provider.Builtin (builtinTools)
import Lambda.Provider.Mcp (startAndLoadMcpServers)
import Lambda.Server.Acp.Types
import Lambda.Types

data AcpServerState = AcpServerState
  { acpConfig           :: !(TVar Config)
  , acpActiveEngine     :: !(TVar (Maybe AppEngineState))
  , acpActiveChannels   :: !(TVar (Maybe EngineChannels))
  , acpActiveSessionId  :: !(TVar Text)
  , acpStdoutLock       :: !(MVar ())
  , acpPendingTurnVar   :: !(TVar (Maybe (TMVar AcpSessionPromptResult)))
  , acpPendingReqLock   :: !(MVar ())
  , acpOutputWriter     :: !(BL.ByteString -> IO ())
  , acpDriverOverride   :: !(TVar (Maybe ModelDriver))
  , acpClientCaps       :: !(TVar (Maybe Aeson.Object))
  , acpNextRpcId        :: !(TVar Int)
  , acpPendingClientRpc :: !(TVar (Map Int (TMVar (Either Aeson.Value Aeson.Value))))
  }

initAcpServerState :: Config -> IO AcpServerState
initAcpServerState cfg = initAcpServerStateWithSink cfg defaultSink
  where
    defaultSink bytes = do
      BL.hPut stdout bytes
      BL.hPut stdout "\n"
      hFlush stdout

initAcpServerStateWithSink :: Config -> (BL.ByteString -> IO ()) -> IO AcpServerState
initAcpServerStateWithSink cfg sink = do
  cfgVar    <- newTVarIO cfg
  engVar    <- newTVarIO Nothing
  chanVar   <- newTVarIO Nothing
  sessVar   <- newTVarIO "sess_default"
  lock      <- newMVar ()
  turnVar   <- newTVarIO Nothing
  reqLock   <- newMVar ()
  drvVar    <- newTVarIO Nothing
  capsVar   <- newTVarIO Nothing
  rpcIdVar  <- newTVarIO 1000
  rpcMapVar <- newTVarIO Map.empty
  pure $ AcpServerState
    { acpConfig           = cfgVar
    , acpActiveEngine     = engVar
    , acpActiveChannels   = chanVar
    , acpActiveSessionId  = sessVar
    , acpStdoutLock       = lock
    , acpPendingTurnVar   = turnVar
    , acpPendingReqLock   = reqLock
    , acpOutputWriter     = sink
    , acpDriverOverride   = drvVar
    , acpClientCaps       = capsVar
    , acpNextRpcId        = rpcIdVar
    , acpPendingClientRpc = rpcMapVar
    }

-- | Sends a JSON-RPC 2.0 response to the configured output sink
sendAcpResponse :: AcpServerState -> Aeson.Value -> Maybe Aeson.Value -> Maybe Aeson.Value -> IO ()
sendAcpResponse state rId mRes mErr = withMVar (acpStdoutLock state) $ \() -> do
  let resp = AcpJsonRpcResponse rId mRes mErr
      bytes = Aeson.encode resp
  acpOutputWriter state bytes

-- | Sends a JSON-RPC 2.0 notification to the configured output sink
sendAcpNotification :: AcpServerState -> Text -> Aeson.Value -> IO ()
sendAcpNotification state method params = withMVar (acpStdoutLock state) $ \() -> do
  let notif = AcpJsonRpcNotification method params
      bytes = Aeson.encode notif
  acpOutputWriter state bytes

-- | Sends a session/update notification to stdout
sendSessionUpdate :: AcpServerState -> Text -> AcpUpdate -> IO ()
sendSessionUpdate state sId upd = do
  let notif = AcpSessionUpdateNotification sId upd
  sendAcpNotification state "session/update" (Aeson.toJSON notif)

-- | Initiates a server-to-client JSON-RPC request and awaits response
callClientRpc :: AcpServerState -> Text -> Aeson.Value -> IO (Either Aeson.Value Aeson.Value)
callClientRpc state method params = do
  (rpcId, replyVar) <- atomically $ do
    curId <- readTVar (acpNextRpcId state)
    writeTVar (acpNextRpcId state) (curId + 1)
    v <- newEmptyTMVar
    modifyTVar' (acpPendingClientRpc state) (Map.insert curId v)
    pure (curId, v)
  let reqObj = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id"      .= rpcId
        , "method"  .= method
        , "params"  .= params
        ]
  withMVar (acpStdoutLock state) $ \() ->
    acpOutputWriter state (Aeson.encode reqObj)
  -- Await client response with timeout of 10s
  mRes <- timeout (10 * 1000 * 1000) (atomically $ takeTMVar replyVar)
  case mRes of
    Just res -> pure res
    Nothing  -> do
      atomically $ modifyTVar' (acpPendingClientRpc state) (Map.delete rpcId)
      pure (Left (Aeson.String "Client RPC timeout"))

-- | Handles an incoming JSON-RPC response fulfilling a server-to-client request
handleAcpResponse :: AcpServerState -> AcpJsonRpcResponse -> IO ()
handleAcpResponse state AcpJsonRpcResponse{..} = do
  case Aeson.fromJSON respId of
    Aeson.Success (i :: Int) -> do
      mVar <- atomically $ do
        pending <- readTVar (acpPendingClientRpc state)
        case Map.lookup i pending of
          Just v -> do
            writeTVar (acpPendingClientRpc state) (Map.delete i pending)
            pure (Just v)
          Nothing -> pure Nothing
      case mVar of
        Just v -> do
          let outcome = case respError of
                Just err -> Left err
                Nothing  -> Right (fromMaybe Aeson.Null respResult)
          atomically $ putTMVar v outcome
        Nothing -> pure ()
    Aeson.Error _ -> pure ()

-- | Converts appStateVector text into structured ACP plan entries
parsePlanEntriesFromVector :: Text -> [AcpPlanEntry]
parsePlanEntriesFromVector txt =
  let ls = filter (not . T.null . T.strip) (T.lines txt)
      toEntry line =
        let (label, rest) = T.breakOn ":" line
            val = T.strip (T.drop 1 rest)
            status = if "DONE" `T.isInfixOf` T.toUpper val || "COMPLETED" `T.isInfixOf` T.toUpper val
                     then "completed"
                     else if "AWAITING" `T.isInfixOf` T.toUpper val || "NONE" `T.isInfixOf` T.toUpper val
                     then "pending"
                     else "in_progress"
            prio = if "GOAL" `T.isPrefixOf` T.toUpper label then Just "high" else Just "medium"
        in AcpPlanEntry line status prio
  in map toEntry ls

-- | Calculates rough prompt and completion token counts from session turns
calculateSessionTokens :: [Turn] -> (Int, Int)
calculateSessionTokens turns =
  let countChars blks = sum [ T.length t | TextBlock t <- blks ]
      promptChars = sum [ countChars (turnBlocks t) | t <- turns, turnRole t /= AssistantRole ]
      compChars   = sum [ countChars (turnBlocks t) | t <- turns, turnRole t == AssistantRole ]
      pToks = max 1 (promptChars `div` 4)
      cToks = max 1 (compChars `div` 4)
  in (pToks + cToks, pToks)

-- | Starts a new session or sets up workspace
startAcpSession :: AcpServerState -> Maybe FilePath -> IO Text
startAcpSession state mCwd = do
  baseCfg <- readTVarIO (acpConfig state)
  let effectiveRoot = fromMaybe (workspaceRoot baseCfg) mCwd
  cfg <- loadConfig effectiveRoot
  atomically $ writeTVar (acpConfig state) cfg

  -- 1. Initialize tool registry with builtins and MCP servers
  let builtinReg = registerTools (builtinTools (workspaceRoot cfg) (artifactDir cfg)) emptyRegistry
  (_mcpClients, mcpTools) <- startAndLoadMcpServers (mcpServers cfg)
  let baseRegistry = registerTools mcpTools builtinReg

  -- 2. Initialize security state
  secState <- initSecurityWithRoot (workspaceRoot cfg) (alwaysAllowGlobs cfg) (alwaysDenyGlobs cfg)

  -- 3. Initialize engine state and model driver
  engineState <- initEngineStateWithSession cfg baseRegistry secState Nothing
  mOverrideDriver <- readTVarIO (acpDriverOverride state)
  driver <- case mOverrideDriver of
    Just d  -> pure d
    Nothing -> openAiDynamicDriver cfg (readTVarIO (appActiveModel engineState))

  -- 4. Register SubAgent spawning tool
  let fullRegistry = registerTool (spawnSpecialistSubAgentTool engineState driver) baseRegistry
  atomically $ writeTVar (appToolRegistry engineState) fullRegistry

  -- 5. Setup frontend/engine channels
  channels <- initEngineChannels (appEventQueue engineState)
  startEngineLoop engineState driver channels

  sess <- readTVarIO (appSession engineState)
  let sId = sessionId sess
  atomically $ do
    writeTVar (acpActiveEngine state) (Just engineState)
    writeTVar (acpActiveChannels state) (Just channels)
    writeTVar (acpActiveSessionId state) sId

  -- 6. Forward engine events to ACP session/update
  _ <- forkIO $ forwardEngineEvents state channels sId

  -- 7. Broadcast available commands update and initial title
  sendSessionUpdate state sId $ AcpAvailableCommandsUpdate
    [ AcpCommand "plan" "Switch to Plan mode (read-only inspection, safe execution)"
    , AcpCommand "exec" "Switch to Exec mode (full tool execution access)"
    , AcpCommand "model" "Inspect or switch active model"
    , AcpCommand "compact" "Compact conversation history"
    , AcpCommand "help" "Show command reference"
    ]
  sendSessionUpdate state sId (AcpSessionInfoUpdate (sessionTitle sess))

  pure sId

-- | Event forwarder bridging EngineEvents to ACP session/update notifications
forwardEngineEvents :: AcpServerState -> EngineChannels -> Text -> IO ()
forwardEngineEvents state channels sId = do
  ev <- atomically $ readTQueue (evQueue channels)
  case ev of
    EvStreamChunk (ChunkText txt) -> do
      sendSessionUpdate state sId (AcpAgentMessageChunk txt)
      forwardEngineEvents state channels sId

    EvStreamChunk (ChunkThinking thk) -> do
      sendSessionUpdate state sId (AcpAgentThoughtChunk thk)
      forwardEngineEvents state channels sId

    EvPermissionRequired prompt -> do
      let callVal = Aeson.object
            [ "toolCallId" .= ("perm_" <> T.pack (show (promptId prompt)))
            , "title"      .= promptTool prompt
            , "kind"       .= ("execute" :: Text)
            , "arguments"  .= promptArgs prompt
            ]
          params = Aeson.object
            [ "sessionId" .= sId
            , "toolCall"  .= callVal
            , "options"   .=
                [ Aeson.object ["optionId" .= ("allow_once" :: Text), "label" .= ("Allow Once" :: Text), "kind" .= ("allow_once" :: Text)]
                , Aeson.object ["optionId" .= ("allow_always" :: Text), "label" .= ("Always Allow" :: Text), "kind" .= ("allow_always" :: Text)]
                , Aeson.object ["optionId" .= ("deny" :: Text), "label" .= ("Deny" :: Text), "kind" .= ("deny" :: Text)]
                ]
            ]
      _ <- forkIO $ do
        clientRes <- callClientRpc state "session/request_permission" params
        case clientRes of
          Right resVal -> do
            let isAllowed = case resVal of
                  Aeson.Object o ->
                    case parseEither (.: "outcome") o of
                      Right (Aeson.String "allow") -> True
                      Right (Aeson.Object oc) -> case parseEither (.: "outcome") oc of
                        Right (Aeson.String "allow") -> True
                        _ -> False
                      _ -> case parseEither (.: "optionId") o of
                        Right (Aeson.String "allow_once")   -> True
                        Right (Aeson.String "allow_always") -> True
                        _ -> False
                  _ -> False
            if isAllowed
              then resolvePrompt prompt PermSession
              else resolvePrompt prompt PermDeny
          Left _ ->
            resolvePrompt prompt PermSession
      forwardEngineEvents state channels sId

    EvTurnUpdated (Turn _ AssistantRole blks) -> do
      mapM_ (\case
        ToolCallBlock tc ->
          sendSessionUpdate state sId (AcpToolCall (toolCallId tc) (toolCallName tc) "execute" "in_progress")
        _ -> pure ()
        ) blks
      forwardEngineEvents state channels sId

    EvTurnUpdated (Turn _ ToolRole blks) -> do
      mapM_ (\case
        ToolResultBlock tr ->
          sendSessionUpdate state sId (AcpToolCallUpdate (resultCallIdRef tr) "completed" (Just (resultStdout tr)))
        _ -> pure ()
        ) blks
      forwardEngineEvents state channels sId

    EvDone -> do
      mEng <- readTVarIO (acpActiveEngine state)
      case mEng of
        Just eng -> do
          turns <- readTVarIO (appTurns eng)
          let (totalToks, promptToks) = calculateSessionTokens turns
              cost = fromIntegral totalToks * 0.000005
          sendSessionUpdate state sId (AcpUsageUpdate totalToks promptToks (Just cost))

          stVec <- readTVarIO (appStateVector eng)
          let entries = parsePlanEntriesFromVector stVec
          unless (null entries) $
            sendSessionUpdate state sId (AcpPlanUpdate entries)
        Nothing -> pure ()

      mVar <- atomically $ do
        v <- readTVar (acpPendingTurnVar state)
        writeTVar (acpPendingTurnVar state) Nothing
        pure v
      case mVar of
        Just doneVar -> do
          isInterrupted <- case mEng of
            Just eng -> readTVarIO (appInterrupted eng)
            Nothing  -> pure False
          let stopReason = if isInterrupted then "cancelled" else "end_turn"
          atomically $ putTMVar doneVar (AcpSessionPromptResult stopReason)
        Nothing      -> pure ()
      forwardEngineEvents state channels sId

    EvError err -> do
      mVar <- atomically $ do
        v <- readTVar (acpPendingTurnVar state)
        writeTVar (acpPendingTurnVar state) Nothing
        pure v
      case mVar of
        Just doneVar -> atomically $ putTMVar doneVar (AcpSessionPromptResult ("error: " <> err))
        Nothing      -> pure ()
      forwardEngineEvents state channels sId

    _ -> forwardEngineEvents state channels sId

-- | Handles an incoming ACP JSON-RPC 2.0 request or notification
handleAcpRequest :: AcpServerState -> AcpJsonRpcRequest -> IO ()
handleAcpRequest state AcpJsonRpcRequest{..} = do
  case reqMethod of
    "initialize" -> do
      let clientCaps = reqParams >>= \p -> case Aeson.fromJSON p of
            Aeson.Success (ip :: AcpInitializeParams) -> initClientCapabilities ip
            Aeson.Error _                            -> Nothing
      atomically $ writeTVar (acpClientCaps state) clientCaps

      let sessCaps = AcpSessionCapabilities
            { capList            = Just mempty
            , capDelete          = Just mempty
            , capClose           = Just mempty
            , capSetConfigOption = Just mempty
            }
          result = AcpInitializeResult
            { initRespProtocolVersion = 1
            , initRespAgentInfo       = AcpAgentInfo "lambdA" "0.1.0.0"
            , initRespCapabilities    = AcpAgentCapabilities
                { capLoadSession         = True
                , capPromptCapabilities  = Just (Aeson.object
                    [ "image"    .= False
                    , "audio"    .= False
                    , "resource" .= False
                    ])
                , capSessionCapabilities = Just sessCaps
                }
            , initRespAuthMethods     = []
            }
      case reqId of
        Just rId -> sendAcpResponse state rId (Just (Aeson.toJSON result)) Nothing
        Nothing  -> pure ()

    "session/new" -> do
      let mParams = case reqParams of
            Just v  -> case Aeson.fromJSON v of
              Aeson.Success (p :: AcpSessionNewParams) -> Just p
              Aeson.Error _                           -> Nothing
            Nothing -> Nothing
          mCwd = mParams >>= newSessionCwd
      sId <- startAcpSession state mCwd
      case reqId of
        Just rId -> sendAcpResponse state rId (Just (Aeson.toJSON (AcpSessionNewResult sId))) Nothing
        Nothing  -> pure ()

    "session/list" -> do
      case reqId of
        Just rId -> do
          let mParams = case reqParams of
                Just v  -> case Aeson.fromJSON v of
                  Aeson.Success (p :: AcpSessionListParams) -> Just p
                  Aeson.Error _                            -> Nothing
                Nothing -> Nothing
          baseCfg <- readTVarIO (acpConfig state)
          let effectiveRoot = fromMaybe (workspaceRoot baseCfg) (mParams >>= listSessionCwd)
              sessDir = effectiveRoot </> ".lambda" </> "sessions"
          exists <- doesDirectoryExist sessDir
          metas <- if exists
            then catch (listMeaningfulSessions sessDir) (\(_ :: SomeException) -> pure [])
            else pure []
          let infos = map (\m -> AcpSessionInfo
                { infoSessionId = metaId m
                , infoCwd       = Just effectiveRoot
                , infoTitle     = Just (metaTitle m)
                , infoUpdatedAt = Just (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (metaUpdatedAt m)))
                }) metas
              result = AcpSessionListResult infos Nothing
          sendAcpResponse state rId (Just (Aeson.toJSON result)) Nothing
        Nothing -> pure ()

    "session/delete" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSessionDeleteParams sId) -> do
              baseCfg <- readTVarIO (acpConfig state)
              let sessDir = workspaceRoot baseCfg </> ".lambda" </> "sessions"
                  jsonFile = sessDir </> (T.unpack sId <.> "json")
                  traceFile = sessDir </> (T.unpack sId <.> "trace.md")
              _ <- try (removeFile jsonFile) :: IO (Either SomeException ())
              _ <- try (removeFile traceFile) :: IO (Either SomeException ())
              sendAcpResponse state rId (Just (Aeson.object ["status" .= ("deleted" :: Text)])) Nothing
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    "session/close" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSessionCloseParams _sId) -> do
              mChannels <- readTVarIO (acpActiveChannels state)
              case mChannels of
                Just ch -> atomically $ writeTBQueue (cmdQueue ch) CmdInterrupt
                Nothing -> pure ()
              atomically $ do
                writeTVar (acpActiveEngine state) Nothing
                writeTVar (acpActiveChannels state) Nothing
              sendAcpResponse state rId (Just (Aeson.object ["status" .= ("closed" :: Text)])) Nothing
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    "session/set_config_option" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSetConfigOptionParams _sId cfgId val) -> do
              case cfgId of
                "model" -> case val of
                  Aeson.String mName -> do
                    mEng <- readTVarIO (acpActiveEngine state)
                    case mEng of
                      Just eng -> atomically $ writeTVar (appActiveModel eng) mName
                      Nothing  -> pure ()
                    atomically $ modifyTVar' (acpConfig state) (\c -> c { modelName = mName })
                    sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text)])) Nothing
                  _ ->
                    sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= ("Expected string model name" :: Text) ]))
                _ ->
                  sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text)])) Nothing
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    "session/prompt" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSessionPromptParams _sId prompt) -> do
              mChannels <- readTVarIO (acpActiveChannels state)
              case mChannels of
                Just channels -> do
                  doneVar <- newEmptyTMVarIO
                  atomically $ writeTVar (acpPendingTurnVar state) (Just doneVar)
                  atomically $ writeTBQueue (cmdQueue channels) (CmdUserPrompt prompt)
                  -- Await prompt turn completion
                  res <- atomically $ takeTMVar doneVar
                  sendAcpResponse state rId (Just (Aeson.toJSON res)) Nothing
                Nothing -> do
                  -- Auto-initialize if no active session
                  _ <- startAcpSession state Nothing
                  handleAcpRequest state (AcpJsonRpcRequest (Just rId) "session/prompt" (Just v))
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= ("Invalid params: " ++ err) ]))
        (Just rId, Nothing) ->
          sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= ("Missing params" :: Text) ]))
        _ -> pure ()

    "session/set_mode" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSetModeParams sId modeStr) -> do
              mEngine <- readTVarIO (acpActiveEngine state)
              case mEngine of
                Just engine -> do
                  let targetMode = if T.toLower modeStr == "plan" then PlanMode else ExecMode
                  atomically $ writeTVar (appMode engine) targetMode
                  sendSessionUpdate state sId (AcpCurrentModeUpdate (if targetMode == PlanMode then "plan" else "exec"))
                  sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text)])) Nothing
                Nothing ->
                  sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text)])) Nothing
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    "session/cancel" -> do
      mChannels <- readTVarIO (acpActiveChannels state)
      case mChannels of
        Just channels -> atomically $ writeTBQueue (cmdQueue channels) CmdInterrupt
        Nothing       -> pure ()
      case reqId of
        Just rId -> sendAcpResponse state rId (Just (Aeson.object ["status" .= ("cancelled" :: Text)])) Nothing
        Nothing  -> pure ()

    "session/load" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSessionLoadParams sId mCwd) -> do
              baseCfg <- readTVarIO (acpConfig state)
              let effectiveRoot = fromMaybe (workspaceRoot baseCfg) mCwd
                  sessDir = effectiveRoot </> ".lambda" </> "sessions"
              loadRes <- loadSession sessDir sId
              case loadRes of
                Right _sess -> do
                  -- Restored successfully
                  _ <- startAcpSession state mCwd
                  sendAcpResponse state rId (Just (Aeson.object ["sessionId" .= sId, "status" .= ("loaded" :: Text)])) Nothing
                Left err ->
                  sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32000 :: Int), "message" .= err ]))
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    _ -> do
      case reqId of
        Just rId ->
          sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32601 :: Int), "message" .= ("Method not found: " <> reqMethod) ]))
        Nothing  -> pure ()

-- | Primary ACP server loop reading from stdin and writing to stdout
runAcpServer :: Config -> IO ()
runAcpServer baseCfg = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  state <- initAcpServerState baseCfg
  TIO.hPutStrLn stderr "[lambdA-ACP] Server initialized. Listening on stdin/stdout..."
  acpLoop state

acpLoop :: AcpServerState -> IO ()
acpLoop state = do
  eof <- isEOF
  unless eof $ do
    lineRes <- try TIO.getLine :: IO (Either SomeException Text)
    case lineRes of
      Left _ -> pure ()
      Right line -> do
        unless (T.null (T.strip line)) $ do
          case Aeson.decode (BL.fromStrict (TE.encodeUtf8 line)) of
            Just (IncomingRequest req)   -> handleAcpRequest state req
            Just (IncomingResponse resp) -> handleAcpResponse state resp
            Nothing                      -> TIO.hPutStrLn stderr ("[lambdA-ACP] Invalid JSON line: " <> line)
        acpLoop state
