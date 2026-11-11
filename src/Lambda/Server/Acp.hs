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
  , startAcpSessionWithSession
  , formatToolCallTitle
  , determineToolKind
  , broadcastUsageUpdate
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, try, finally)
import Control.Monad (unless, when)
import Data.Foldable (for_, traverse_)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
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
import Text.Read (readMaybe)
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

import Data.Containers.ListUtils (nubOrd)

import Lambda.Config (Config(..), loadConfig, curatedModels, lookupModelContextLimitWithConfig, isOpenRouterEndpoint)
import Lambda.Driver.OpenAI (fetchEndpointModels)
import Lambda.Core.EngineInterface
  ( EngineChannels(..)
  , EngineRuntime(..)
  , startEngineRuntime
  , stopEngineRuntime
  )
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Engine.Compactor (estimateTotalTokens)
import Lambda.Engine.Security (resolvePrompt)
import Lambda.Engine.Session (loadSession, listMeaningfulSessions, Session(..), SessionMeta(..))
import Lambda.Engine.State (AppEngineState(..))
import Lambda.Server.Acp.Types
import Lambda.Types

data AcpServerState = AcpServerState
  { acpConfig             :: !(TVar Config)
  , acpActiveRuntime      :: !(TVar (Maybe EngineRuntime))
  , acpActiveEngine       :: !(TVar (Maybe AppEngineState))
  , acpActiveChannels     :: !(TVar (Maybe EngineChannels))
  , acpActiveSessionId    :: !(TVar Text)
  , acpStdoutLock         :: !(MVar ())
  , acpPendingTurnVar     :: !(TVar (Maybe (TMVar AcpSessionPromptResult)))
  , acpPendingReqLock     :: !(MVar ())
  , acpOutputWriter       :: !(BL.ByteString -> IO ())
  , acpDriverOverride     :: !(TVar (Maybe ModelDriver))
  , acpClientCaps         :: !(TVar (Maybe Aeson.Object))
  , acpNextRpcId          :: !(TVar Int)
  , acpPendingClientRpc   :: !(TVar (Map Int (TMVar (Either Aeson.Value Aeson.Value))))
  , acpActiveSubAgentCallId :: !(TVar (Maybe Text))
  , acpForwarderThread    :: !(TVar (Maybe ThreadId))
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
  rtVar     <- newTVarIO Nothing
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
  subCallIdVar <- newTVarIO Nothing
  fwdThreadVar <- newTVarIO Nothing
  -- Asynchronously discover models from endpoint if configured
  _ <- forkIO $ do
    detected <- fetchEndpointModels cfg
    unless (null detected) $ atomically $ modifyTVar' cfgVar $ \c ->
      c { configuredModels = nubOrd (configuredModels c ++ detected) }
  pure $ AcpServerState
    { acpConfig             = cfgVar
    , acpActiveRuntime      = rtVar
    , acpActiveEngine       = engVar
    , acpActiveChannels     = chanVar
    , acpActiveSessionId    = sessVar
    , acpStdoutLock         = lock
    , acpPendingTurnVar     = turnVar
    , acpPendingReqLock     = reqLock
    , acpOutputWriter       = sink
    , acpDriverOverride     = drvVar
    , acpClientCaps         = capsVar
    , acpNextRpcId          = rpcIdVar
    , acpPendingClientRpc   = rpcMapVar
    , acpActiveSubAgentCallId = subCallIdVar
    , acpForwarderThread    = fwdThreadVar
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

-- | Initiates a server-to-client JSON-RPC request and awaits response indefinitely
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
  -- Await client response indefinitely until answered or cancelled
  atomically $ takeTMVar replyVar

-- | Handles an incoming JSON-RPC response fulfilling a server-to-client request
handleAcpResponse :: AcpServerState -> AcpJsonRpcResponse -> IO ()
handleAcpResponse state AcpJsonRpcResponse{..} = do
  let mId = case Aeson.fromJSON respId of
        Aeson.Success (i :: Int) -> Just i
        Aeson.Error _ -> case respId of
          Aeson.String s -> readMaybe (T.unpack s)
          _              -> Nothing
  case mId of
    Just i -> do
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
    Nothing -> pure ()

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

-- | Calculates prompt and completion token counts across dialogue turns
calculateSessionTokens :: [Turn] -> (Int, Int)
calculateSessionTokens turns =
  let promptToks = estimateTotalTokens (filter (\t -> turnRole t /= AssistantRole) turns)
      compToks   = estimateTotalTokens (filter (\t -> turnRole t == AssistantRole) turns)
  in (promptToks + compToks, promptToks)

-- | Broadcasts real-time context token usage and model capacity to the ACP client (e.g. Zed)
broadcastUsageUpdate :: AcpServerState -> AppEngineState -> Text -> IO ()
broadcastUsageUpdate state eng sId = do
  turns <- readTVarIO (appTurns eng)
  ctxLimit <- readTVarIO (appContextLimit eng)
  let (totalToks, promptToks) = calculateSessionTokens turns
      compToks = max 0 (totalToks - promptToks)
  sendSessionUpdate state sId (AcpUsageUpdate totalToks ctxLimit Nothing (Just promptToks) (Just compToks))

-- | Formats a descriptive, human-readable title for a tool call based on its arguments
formatToolCallTitle :: ToolCall -> Text
formatToolCallTitle tc =
  let name = toolCallName tc
      argsObj = case toolCallArgs tc of
        Aeson.Object o -> Just o
        _              -> Nothing
      detail = case (name, argsObj) of
        ("read_file", Just o) ->
          case parseEither (.: "path") o of
            Right (p :: Text) -> " " <> p
            _ -> ""
        ("list_directory", Just o) ->
          case parseEither (.:? "path") o of
            Right (Just (p :: Text)) -> " " <> p
            _ -> ""
        ("execute_bash", Just o) ->
          case parseEither (.: "command") o of
            Right (c :: Text) -> " " <> (if T.length c > 50 then T.take 47 c <> "..." else c)
            _ -> ""
        ("call_mcp_tool", Just o) ->
          let sName = case parseEither (.: "server_name") o of
                Right (s :: Text) -> Right s
                Left _            -> parseEither (.: "ServerName") o
              tName = case parseEither (.: "tool_name") o of
                Right (t :: Text) -> Right t
                Left _            -> parseEither (.: "ToolName") o
              mArgs = case parseEither (.:? "arguments") o of
                Right (Just (Aeson.Object io)) -> Right (Just (Aeson.Object io))
                _                              -> parseEither (.:? "Arguments") o
          in case (sName, tName) of
               (Right (s :: Text), Right (t :: Text)) ->
                 let inner = case mArgs of
                       Right (Just (Aeson.Object io)) ->
                         let mPath = parseEither (.:? "path") io
                             mAct  = parseEither (.:? "action") io
                             mQry  = parseEither (.:? "query") io
                             mTgt  = parseEither (.:? "target_file") io
                         in case (mPath, mAct, mQry, mTgt) of
                              (Right (Just (p :: Text)), _, _, _) -> " (" <> p <> ")"
                              (_, Right (Just (a :: Text)), _, _) -> " (" <> a <> ")"
                              (_, _, Right (Just (q :: Text)), _) -> " (" <> q <> ")"
                              (_, _, _, Right (Just (tf :: Text))) -> " (" <> tf <> ")"
                              _ -> ""
                       _ -> ""
                 in " " <> s <> "/" <> t <> inner
               _ -> ""
        ("spawn_specialist_subagent", Just o) ->
          case (parseEither (.: "role") o, parseEither (.: "task") o) of
            (Right (r :: Text), Right (t :: Text)) ->
              " " <> r <> " (" <> (if T.length t > 40 then T.take 37 t <> "..." else t) <> ")"
            (Right (r :: Text), _) -> " " <> r
            _ -> ""
        ("grep_search", Just o) ->
          case parseEither (.: "pattern") o of
            Right (pat :: Text) -> " \"" <> pat <> "\""
            _ -> ""
        ("file_search", Just o) ->
          case parseEither (.: "query") o of
            Right (q :: Text) -> " \"" <> q <> "\""
            _ -> ""
        ("edit_file", Just o) ->
          case parseEither (.: "path") o of
            Right (p :: Text) -> " " <> p
            _ -> ""
        ("write_file", Just o) ->
          case parseEither (.: "path") o of
            Right (p :: Text) -> " " <> p
            _ -> ""
        ("fetch_url", Just o) ->
          case parseEither (.: "url") o of
            Right (u :: Text) -> " " <> u
            _ -> ""
        _ -> ""
  in name <> detail

-- | Determines ACP tool kind (read, edit, execute)
determineToolKind :: Text -> Text
determineToolKind = \case
  "read_file"      -> "read"
  "list_directory" -> "read"
  "grep_search"    -> "read"
  "file_search"    -> "read"
  "fetch_web"      -> "read"
  "fetch_url"      -> "read"
  "edit_file"      -> "edit"
  "write_file"     -> "edit"
  _                -> "execute"

-- | Standard commands advertised to ACP clients
acpStandardCommands :: [AcpCommand]
acpStandardCommands =
  [ AcpCommand "plan" "Switch to Plan mode (read-only inspection, safe execution)"
  , AcpCommand "exec" "Switch to Exec mode (full tool execution access)"
  , AcpCommand "model" "Inspect or switch active model"
  , AcpCommand "compact" "Compact conversation history"
  , AcpCommand "clear" "Clear session history"
  , AcpCommand "commands" "List all available slash commands"
  , AcpCommand "help" "Show command reference"
  ]

-- | Builds ACP config options for model selection, mode switching, and reasoning level
buildSessionConfigOptions :: Config -> AgentMode -> Text -> Text -> [AcpConfigOption]
buildSessionConfigOptions cfg curMode curModel curThought =
  let modelChoices =
        let curChoice = [AcpConfigOptionChoice curModel curModel (Just "Active model") | not (T.null curModel)]
            showCurated = null (configuredModels cfg) && isOpenRouterEndpoint (apiBaseUrl cfg)
            curated = if showCurated
                        then [ AcpConfigOptionChoice m m (Just "Curated model option") | m <- curatedModels, m /= curModel ]
                        else []
            cfgCustom = [ AcpConfigOptionChoice m m (Just "Configured model") | m <- configuredModels cfg, m /= curModel ]
        in curChoice ++ cfgCustom ++ curated
      modeChoices =
        [ AcpConfigOptionChoice "plan" "Plan Mode" (Just "Read-only inspection and safe planning")
        , AcpConfigOptionChoice "exec" "Exec Mode" (Just "Full mutating tool execution access")
        ]
      thoughtChoices =
        [ AcpConfigOptionChoice "low" "Low" (Just "Concise responses")
        , AcpConfigOptionChoice "medium" "Medium" (Just "Balanced reasoning")
        , AcpConfigOptionChoice "high" "High" (Just "In-depth deductive reasoning")
        ]
      modeStr = if curMode == PlanMode then "plan" else "exec"
  in [ AcpConfigOption "model" "Model" (Just "Active language model") (Just "model") "select" curModel modelChoices
     , AcpConfigOption "mode" "Mode" (Just "Operating mode") (Just "mode") "select" modeStr modeChoices
     , AcpConfigOption "thought_level" "Reasoning Level" (Just "Model reasoning effort") (Just "thought_level") "select" curThought thoughtChoices
     ]

-- | Builds legacy modes structure for ACP v1 client backward compatibility
buildSessionModes :: AgentMode -> AcpModesState
buildSessionModes curMode =
  let modeStr = if curMode == PlanMode then "plan" else "exec"
  in AcpModesState
       { currentModeId  = modeStr
       , availableModes =
           [ AcpModeInfo "plan" "Plan" (Just "Read-only inspection and safe planning")
           , AcpModeInfo "exec" "Exec" (Just "Full mutating tool execution access")
           ]
       }

-- | Handles ACP slash commands intercepted before reaching LLM
handleSlashPrompt :: AcpServerState -> Maybe Aeson.Value -> Text -> Text -> Maybe EngineChannels -> Maybe AppEngineState -> Config -> IO ()
handleSlashPrompt state mReqId sId prompt mChannels mEngine baseCfg = do
  let (cmdWord, rawArgs) = T.breakOn " " prompt
      cmd = T.toLower cmdWord
      args = T.strip rawArgs
      finish = case mReqId of
        Just rId -> sendAcpResponse state rId (Just (Aeson.toJSON (AcpSessionPromptResult "end_turn"))) Nothing
        Nothing  -> pure ()
  case cmd of
    "/plan" -> do
      for_ mEngine $ \eng -> atomically $ writeTVar (appMode eng) PlanMode
      for_ mChannels $ \ch -> atomically $ writeTBQueue (cmdQueue ch) (CmdSetMode PlanMode)
      curModel <- case mEngine of
        Just eng -> readTVarIO (appActiveModel eng)
        Nothing  -> pure (modelName baseCfg)
      sendSessionUpdate state sId (AcpCurrentModeUpdate "plan")
      sendSessionUpdate state sId $ AcpConfigOptionsUpdate
        (buildSessionConfigOptions baseCfg PlanMode curModel "high")
      sendSessionUpdate state sId (AcpAgentMessageChunk "Switched to **Plan Mode** (read-only inspection, safe execution).\n")
      finish

    "/exec" -> do
      for_ mEngine $ \eng -> atomically $ writeTVar (appMode eng) ExecMode
      for_ mChannels $ \ch -> atomically $ writeTBQueue (cmdQueue ch) (CmdSetMode ExecMode)
      curModel <- case mEngine of
        Just eng -> readTVarIO (appActiveModel eng)
        Nothing  -> pure (modelName baseCfg)
      sendSessionUpdate state sId (AcpCurrentModeUpdate "exec")
      sendSessionUpdate state sId $ AcpConfigOptionsUpdate
        (buildSessionConfigOptions baseCfg ExecMode curModel "high")
      sendSessionUpdate state sId (AcpAgentMessageChunk "Switched to **Exec Mode** (full tool execution access).\n")
      finish

    "/model" -> do
      if T.null args
        then do
          curModel <- case mEngine of
            Just eng -> readTVarIO (appActiveModel eng)
            Nothing  -> pure (modelName baseCfg)
          cfg <- readTVarIO (acpConfig state)
          let availModels = if not (null (configuredModels cfg))
                              then configuredModels cfg
                              else if isOpenRouterEndpoint (apiBaseUrl cfg)
                                     then curatedModels
                                     else []
              modelsMsg = "Active model: `" <> curModel <> "`\n"
                <> if null availModels
                     then "\nTo switch models, run `/model <model-name>`\n"
                     else "\nAvailable models:\n"
                          <> T.unlines [ "- `" <> m <> "`" | m <- availModels ]
                          <> "\nTo switch models, run `/model <model-name>`\n"
          sendSessionUpdate state sId (AcpAgentMessageChunk modelsMsg)
          finish
        else do
          let targetModel = args
          cfg <- readTVarIO (acpConfig state)
          let targetLimit = lookupModelContextLimitWithConfig cfg targetModel
          case mEngine of
            Just eng -> atomically $ do
              writeTVar (appActiveModel eng) targetModel
              writeTVar (appContextLimit eng) targetLimit
            Nothing  -> pure ()
          case mChannels of
            Just ch -> atomically $ writeTBQueue (cmdQueue ch) (CmdSetModel targetModel)
            Nothing -> pure ()
          let updatedCfg = cfg { modelName = targetModel, contextWindowLimit = targetLimit }
          atomically $ writeTVar (acpConfig state) updatedCfg
          curMode <- case mEngine of
            Just eng -> readTVarIO (appMode eng)
            Nothing  -> pure PlanMode
          sendSessionUpdate state sId $ AcpConfigOptionsUpdate
            (buildSessionConfigOptions updatedCfg curMode targetModel "high")
          case mEngine of
            Just eng -> broadcastUsageUpdate state eng sId
            Nothing  -> pure ()
          sendSessionUpdate state sId (AcpAgentMessageChunk ("Active model switched to: `" <> targetModel <> "`\n"))
          finish

    "/compact" -> do
      case mChannels of
        Just ch -> atomically $ writeTBQueue (cmdQueue ch) CmdCompactHistory
        Nothing -> pure ()
      case mEngine of
        Just eng -> broadcastUsageUpdate state eng sId
        Nothing  -> pure ()
      sendSessionUpdate state sId (AcpAgentMessageChunk "Conversation history compaction initiated.\n")
      finish

    "/clear" -> do
      case mChannels of
        Just ch -> atomically $ writeTBQueue (cmdQueue ch) CmdClearHistory
        Nothing -> pure ()
      case mEngine of
        Just eng -> broadcastUsageUpdate state eng sId
        Nothing  -> pure ()
      sendSessionUpdate state sId (AcpAgentMessageChunk "Session history cleared.\n")
      finish

    c | c `elem` ["/help", "/commands"] -> do
      let msg = "Available ACP Slash Commands:\n"
            <> "- `/plan` : Switch to Plan mode (read-only inspection, safe execution)\n"
            <> "- `/exec` : Switch to Exec mode (mutating tool execution access)\n"
            <> "- `/model [name]` : Show active model or switch to a new model\n"
            <> "- `/compact` : Compact conversation history\n"
            <> "- `/clear` : Clear current session history\n"
            <> "- `/commands` or `/help` : Show this command reference\n"
      sendSessionUpdate state sId (AcpAgentMessageChunk msg)
      finish

    _ -> do
      sendSessionUpdate state sId (AcpAgentMessageChunk ("Unrecognized slash command `" <> cmdWord <> "`. Type `/help` for available commands.\n"))
      finish

-- | Starts a new session or sets up workspace
startAcpSession :: AcpServerState -> Maybe FilePath -> IO Text
startAcpSession state mCwd = startAcpSessionWithSession state mCwd Nothing

-- | Starts a session with an optional existing Session restored
startAcpSessionWithSession :: AcpServerState -> Maybe FilePath -> Maybe Session -> IO Text
startAcpSessionWithSession state mCwd maybeSess = do
  baseCfg <- readTVarIO (acpConfig state)
  let effectiveRoot = fromMaybe (workspaceRoot baseCfg) mCwd
  cfg <- loadConfig effectiveRoot
  atomically $ writeTVar (acpConfig state) cfg

  -- Stop any previous runtime and forwarder thread before bootstrapping the new session
  mPrevFwd <- readTVarIO (acpForwarderThread state)
  traverse_ killThread mPrevFwd
  atomically $ writeTVar (acpForwarderThread state) Nothing

  mPrevRuntime <- readTVarIO (acpActiveRuntime state)
  traverse_ stopEngineRuntime mPrevRuntime

  mOverrideDriver <- readTVarIO (acpDriverOverride state)
  runtime <- startEngineRuntime cfg maybeSess mOverrideDriver

  let engineState = erEngineState runtime
      channels    = erChannels runtime

  sess <- readTVarIO (appSession engineState)
  let sId = sessionId sess
  atomically $ do
    writeTVar (acpActiveRuntime state) (Just runtime)
    writeTVar (acpActiveEngine state) (Just engineState)
    writeTVar (acpActiveChannels state) (Just channels)
    writeTVar (acpActiveSessionId state) sId

  -- Forward engine events to ACP session/update
  fwdTid <- forkIO $ forwardEngineEvents state channels sId
  atomically $ writeTVar (acpForwarderThread state) (Just fwdTid)

  -- 7. Broadcast available commands update, mode, config options, and initial title
  sendSessionUpdate state sId $ AcpAvailableCommandsUpdate acpStandardCommands
  let modeStr = if sessionMode sess == PlanMode then "plan" else "exec"
  sendSessionUpdate state sId (AcpCurrentModeUpdate modeStr)
  sendSessionUpdate state sId $ AcpConfigOptionsUpdate
    (buildSessionConfigOptions cfg (sessionMode sess) (modelName cfg) "high")
  sendSessionUpdate state sId (AcpSessionInfoUpdate (sessionTitle sess))
  broadcastUsageUpdate state engineState sId

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

    EvStreamChunk (ChunkError err) -> do
      sendSessionUpdate state sId (AcpAgentMessageChunk ("\n⚠️ " <> err <> "\n"))
      forwardEngineEvents state channels sId

    EvPermissionRequired prompt -> do
      let realCallId = fromMaybe ("perm_" <> T.pack (show (promptId prompt))) (promptToolCallId prompt)
          callVal = Aeson.object
            [ "toolCallId" .= realCallId
            , "title"      .= promptTool prompt
            , "kind"       .= ("execute" :: Text)
            , "status"     .= ("pending" :: Text)
            , "arguments"  .= promptArgs prompt
            , "rawInput"   .= promptArgs prompt
            ]
          params = Aeson.object
            [ "sessionId" .= sId
            , "toolCall"  .= callVal
            , "options"   .=
                [ Aeson.object ["optionId" .= ("allow_once" :: Text), "name" .= ("Allow Once" :: Text), "label" .= ("Allow Once" :: Text), "kind" .= ("allow_once" :: Text)]
                , Aeson.object ["optionId" .= ("allow_always" :: Text), "name" .= ("Always Allow" :: Text), "label" .= ("Always Allow" :: Text), "kind" .= ("allow_always" :: Text)]
                , Aeson.object ["optionId" .= ("reject_once" :: Text), "name" .= ("Deny" :: Text), "label" .= ("Deny" :: Text), "kind" .= ("reject_once" :: Text)]
                ]
            ]
      _ <- forkIO $ do
        clientRes <- callClientRpc state "session/request_permission" params
        case clientRes of
          Right resVal -> do
            let outcome = case resVal of
                  Aeson.Object o ->
                    case parseEither (.: "outcome") o of
                      Right (Aeson.Object oc) ->
                        case parseEither (.: "optionId") oc of
                          Right (opt :: Text) -> Just opt
                          Left _ -> case parseEither (.: "outcome") oc of
                            Right (out :: Text) -> Just out
                            Left _ -> Nothing
                      Right (Aeson.String out) -> Just out
                      _ -> case parseEither (.: "optionId") o of
                        Right (opt :: Text) -> Just opt
                        Left _ -> Nothing
                  _ -> Nothing
            case outcome of
              Just "allow_always" -> resolvePrompt prompt PermAlways
              Just "allow_once"   -> resolvePrompt prompt PermOnce
              Just "allow"        -> resolvePrompt prompt PermOnce
              _                   -> resolvePrompt prompt PermDeny
          Left _ ->
            -- Fail-closed on error or cancellation
            resolvePrompt prompt PermDeny
      forwardEngineEvents state channels sId

    EvTurnUpdated (Turn _ AssistantRole blks) -> do
      mapM_ (\case
        ToolCallBlock tc -> do
          when (toolCallName tc `elem` ["spawn_specialist_subagent", "resume_subagent"]) $
            atomically $ writeTVar (acpActiveSubAgentCallId state) (Just (toolCallId tc))
          sendSessionUpdate state sId
            (AcpToolCall (toolCallId tc)
                         (formatToolCallTitle tc)
                         (determineToolKind (toolCallName tc))
                         "in_progress"
                         (Just (toolCallArgs tc)))
        _ -> pure ()
        ) blks
      forwardEngineEvents state channels sId

    EvTurnUpdated (Turn _ ToolRole blks) -> do
      mapM_ (\case
        ToolResultBlock tr -> do
          atomically $ writeTVar (acpActiveSubAgentCallId state) Nothing
          sendSessionUpdate state sId (AcpToolCallUpdate (resultCallIdRef tr) "completed" (Just (resultStdout tr)))
        _ -> pure ()
        ) blks
      mEng <- readTVarIO (acpActiveEngine state)
      case mEng of
        Just eng -> broadcastUsageUpdate state eng sId
        Nothing  -> pure ()
      forwardEngineEvents state channels sId

    EvDone -> do
      mEng <- readTVarIO (acpActiveEngine state)
      case mEng of
        Just eng -> do
          broadcastUsageUpdate state eng sId

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
          atomically $ do
            _ <- tryPutTMVar doneVar (AcpSessionPromptResult stopReason)
            pure ()
        Nothing      -> pure ()
      forwardEngineEvents state channels sId

    EvSubAgentUpdate task -> do
      mCallId <- readTVarIO (acpActiveSubAgentCallId state)
      case mCallId of
        Just callId -> do
          let statusText = "[" <> subAgentRole task <> "] "
                <> (case subAgentStatus task of
                      SubAgentRunning -> "Turn " <> T.pack (show (subAgentTurnCount task)) <> "/" <> T.pack (show (subAgentBudget task))
                      SubAgentSuccess _ -> "Completed"
                      SubAgentBlocked b -> "Blocked: " <> b
                      SubAgentFailed f -> "Failed: " <> f)
                <> (if T.null (subAgentHypothesis task) then "" else " - " <> T.take 40 (subAgentHypothesis task))
          sendSessionUpdate state sId (AcpToolCallUpdate callId "in_progress" (Just statusText))
        Nothing -> pure ()
      forwardEngineEvents state channels sId

    EvWorkingStateUpdate stVec -> do
      let entries = parsePlanEntriesFromVector stVec
      unless (null entries) $
        sendSessionUpdate state sId (AcpPlanUpdate entries)
      forwardEngineEvents state channels sId

    EvModelSwitched newModel _ -> do
      baseCfg <- readTVarIO (acpConfig state)
      mEng <- readTVarIO (acpActiveEngine state)
      curMode <- case mEng of
        Just eng -> do
          broadcastUsageUpdate state eng sId
          readTVarIO (appMode eng)
        Nothing  -> pure PlanMode
      sendSessionUpdate state sId $ AcpConfigOptionsUpdate
        (buildSessionConfigOptions baseCfg curMode newModel "high")
      forwardEngineEvents state channels sId

    EvError err -> do
      sendSessionUpdate state sId (AcpAgentMessageChunk ("\n[Error: " <> err <> "]\n"))
      mVar <- atomically $ do
        v <- readTVar (acpPendingTurnVar state)
        writeTVar (acpPendingTurnVar state) Nothing
        pure v
      case mVar of
        Just doneVar -> atomically $ do
          _ <- tryPutTMVar doneVar (AcpSessionPromptResult "end_turn")
          pure ()
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
      baseCfg <- readTVarIO (acpConfig state)
      mEng <- readTVarIO (acpActiveEngine state)
      (curMode, curModel) <- case mEng of
        Just eng -> do
          mode <- readTVarIO (appMode eng)
          model <- readTVarIO (appActiveModel eng)
          pure (mode, model)
        Nothing -> pure (PlanMode, modelName baseCfg)
      let cfgOpts = buildSessionConfigOptions baseCfg curMode curModel "high"
          modesSt = buildSessionModes curMode
          res = AcpSessionNewResult
            { newSessionId         = sId
            , newConfigOptions     = Just cfgOpts
            , newModes             = Just modesSt
            , newAvailableCommands = Just acpStandardCommands
            }
      case reqId of
        Just rId -> sendAcpResponse state rId (Just (Aeson.toJSON res)) Nothing
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
              mRuntime <- readTVarIO (acpActiveRuntime state)
              case mRuntime of
                Just rt -> stopEngineRuntime rt
                Nothing -> pure ()
              atomically $ do
                writeTVar (acpActiveRuntime state) Nothing
                writeTVar (acpActiveEngine state) Nothing
                writeTVar (acpActiveChannels state) Nothing
                writeTVar (acpActiveSessionId state) ""
              sendAcpResponse state rId (Just (Aeson.object ["status" .= ("closed" :: Text)])) Nothing
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    "session/set_config_option" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSetConfigOptionParams sId cfgId val) -> do
              baseCfg <- readTVarIO (acpConfig state)
              mEng <- readTVarIO (acpActiveEngine state)
              case cfgId of
                "model" -> case val of
                  Aeson.String mName -> do
                    let targetLimit = lookupModelContextLimitWithConfig baseCfg mName
                    case mEng of
                      Just eng -> atomically $ do
                        writeTVar (appActiveModel eng) mName
                        writeTVar (appContextLimit eng) targetLimit
                      Nothing  -> pure ()
                    atomically $ modifyTVar' (acpConfig state) (\c -> c { modelName = mName, contextWindowLimit = targetLimit })
                    updatedCfg <- readTVarIO (acpConfig state)
                    curMode <- case mEng of
                      Just eng -> readTVarIO (appMode eng)
                      Nothing  -> pure PlanMode
                    let opts = buildSessionConfigOptions updatedCfg curMode mName "high"
                    sendSessionUpdate state sId (AcpConfigOptionsUpdate opts)
                    case mEng of
                      Just eng -> broadcastUsageUpdate state eng sId
                      Nothing  -> pure ()
                    sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text), "configOptions" .= opts])) Nothing
                  _ ->
                    sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= ("Expected string model name" :: Text) ]))
                "mode" -> case val of
                  Aeson.String modeStr -> do
                    let targetMode = if T.toLower modeStr == "exec" then ExecMode else PlanMode
                    case mEng of
                      Just eng -> atomically $ writeTVar (appMode eng) targetMode
                      Nothing  -> pure ()
                    curModel <- case mEng of
                      Just eng -> readTVarIO (appActiveModel eng)
                      Nothing  -> pure (modelName baseCfg)
                    let opts = buildSessionConfigOptions baseCfg targetMode curModel "high"
                    sendSessionUpdate state sId (AcpCurrentModeUpdate (if targetMode == PlanMode then "plan" else "exec"))
                    sendSessionUpdate state sId (AcpConfigOptionsUpdate opts)
                    sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text), "configOptions" .= opts])) Nothing
                  _ ->
                    sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= ("Expected string mode name" :: Text) ]))
                "thought_level" -> case val of
                  Aeson.String thkStr -> do
                    (curMode, curModel) <- case mEng of
                      Just eng -> do
                        m <- readTVarIO (appMode eng)
                        md <- readTVarIO (appActiveModel eng)
                        pure (m, md)
                      Nothing -> pure (PlanMode, modelName baseCfg)
                    let opts = buildSessionConfigOptions baseCfg curMode curModel thkStr
                    sendSessionUpdate state sId (AcpConfigOptionsUpdate opts)
                    sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text), "configOptions" .= opts])) Nothing
                  _ ->
                    sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= ("Expected string thought level" :: Text) ]))
                _ -> do
                  (curMode, curModel) <- case mEng of
                    Just eng -> do
                      m <- readTVarIO (appMode eng)
                      md <- readTVarIO (appActiveModel eng)
                      pure (m, md)
                    Nothing -> pure (PlanMode, modelName baseCfg)
                  let opts = buildSessionConfigOptions baseCfg curMode curModel "high"
                  sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text), "configOptions" .= opts])) Nothing
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    "session/prompt" -> do
      case (reqId, reqParams) of
        (Just rId, Just v) -> do
          case Aeson.fromJSON v of
            Aeson.Success (AcpSessionPromptParams sId prompt) -> do
              mChannels <- readTVarIO (acpActiveChannels state)
              mEngine   <- readTVarIO (acpActiveEngine state)
              baseCfg   <- readTVarIO (acpConfig state)
              let trimmed = T.strip prompt
                  firstLine = T.takeWhile (/= '\n') trimmed
                  (cmdWord, _) = T.breakOn " " firstLine
              if cmdWord `elem` ["/plan", "/exec", "/model", "/compact", "/clear", "/help", "/commands"]
                then handleSlashPrompt state (Just rId) sId trimmed mChannels mEngine baseCfg
                else withMVar (acpPendingReqLock state) $ \() -> do
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
              baseCfg <- readTVarIO (acpConfig state)
              let targetMode = if T.toLower modeStr == "exec" then ExecMode else PlanMode
              curModel <- case mEngine of
                Just engine -> do
                  atomically $ writeTVar (appMode engine) targetMode
                  readTVarIO (appActiveModel engine)
                Nothing -> pure (modelName baseCfg)
              let opts = buildSessionConfigOptions baseCfg targetMode curModel "high"
              sendSessionUpdate state sId (AcpCurrentModeUpdate (if targetMode == PlanMode then "plan" else "exec"))
              sendSessionUpdate state sId (AcpConfigOptionsUpdate opts)
              sendAcpResponse state rId (Just (Aeson.object ["status" .= ("ok" :: Text), "configOptions" .= opts])) Nothing
            Aeson.Error err ->
              sendAcpResponse state rId Nothing (Just (Aeson.object [ "code" .= (-32602 :: Int), "message" .= err ]))
        _ -> pure ()

    m | m `elem` ["session/cancel", "session/stop", "$/cancel_request", "$/cancelRequest"] -> do
      mEngine <- readTVarIO (acpActiveEngine state)
      case mEngine of
        Just eng -> atomically $ do
          writeTVar (appInterrupted eng) True
          subs <- readTVar (appSubAgents eng)
          let updatedSubs = Map.map (\t -> if subAgentStatus t == SubAgentRunning then t { subAgentStatus = SubAgentBlocked "Cancelled by user" } else t) subs
          writeTVar (appSubAgents eng) updatedSubs
        Nothing  -> pure ()
      mChannels <- readTVarIO (acpActiveChannels state)
      case mChannels of
        Just channels -> atomically $ writeTBQueue (cmdQueue channels) CmdInterrupt
        Nothing       -> pure ()
      -- Immediately fulfill any in-flight prompt with "cancelled" so the client stops waiting
      mVar <- atomically $ do
        v <- readTVar (acpPendingTurnVar state)
        writeTVar (acpPendingTurnVar state) Nothing
        pure v
      case mVar of
        Just doneVar -> atomically $ do
          _ <- tryPutTMVar doneVar (AcpSessionPromptResult "cancelled")
          pure ()
        Nothing -> pure ()
      -- Immediately unblock all waiting client RPCs (e.g. permission requests) with cancellation
      atomically $ do
        pendingRpcs <- readTVar (acpPendingClientRpc state)
        writeTVar (acpPendingClientRpc state) Map.empty
        mapM_ (\replyV -> tryPutTMVar replyV (Left (Aeson.String "Cancelled by user"))) (Map.elems pendingRpcs)
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
                Right sess -> do
                  -- Restored successfully
                  _ <- startAcpSessionWithSession state mCwd (Just sess)
                  mEng <- readTVarIO (acpActiveEngine state)
                  (curMode, curModel) <- case mEng of
                    Just eng -> do
                      mode <- readTVarIO (appMode eng)
                      model <- readTVarIO (appActiveModel eng)
                      pure (mode, model)
                    Nothing -> pure (sessionMode sess, modelName baseCfg)
                  let cfgOpts = buildSessionConfigOptions baseCfg curMode curModel "high"
                      modesSt = buildSessionModes curMode
                      res = AcpSessionNewResult
                        { newSessionId         = sId
                        , newConfigOptions     = Just cfgOpts
                        , newModes             = Just modesSt
                        , newAvailableCommands = Just acpStandardCommands
                        }
                  sendAcpResponse state rId (Just (Aeson.toJSON res)) Nothing
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
  acpLoop state `finally` cleanupServer state
  where
    cleanupServer st = do
      mRt <- readTVarIO (acpActiveRuntime st)
      case mRt of
        Just rt -> stopEngineRuntime rt
        Nothing -> pure ()

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
            Just (IncomingRequest req)   -> do
              _ <- forkIO $ handleAcpRequest state req
              pure ()
            Just (IncomingResponse resp) -> handleAcpResponse state resp
            Nothing                      -> TIO.hPutStrLn stderr ("[lambdA-ACP] Invalid JSON line: " <> line)
        acpLoop state
