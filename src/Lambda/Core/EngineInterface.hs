{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Core.EngineInterface
  ( EngineChannels(..)
  , EngineRuntime(..)
  , initEngineChannels
  , startEngineLoop
  , startEngineRuntime
  , stopEngineRuntime
  , runAgentTurnLoop
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently, race)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forever, when, unless)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map.Strict as Map
import Lambda.Config (Config(..), resolveModelWithConfig, lookupModelContextLimit)
import Lambda.Core.ModelDriver (ModelDriver(..))
import Network.HTTP.Client.TLS (newTlsManager)

import Lambda.Core.ToolProvider (emptyRegistry, registerTools, registerTool, toolsToOpenAISchema)
import Lambda.Engine.Compactor (compactHistory, estimateTotalTokens)
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.Security (SecurityState(..), initSecurityWithRoot)
import Lambda.Engine.Session (exportSessionTrace, loadSession, forkSession, deriveTitle, Session(..))
import Lambda.Engine.State
import Lambda.Engine.SubAgent (spawnSpecialistSubAgentTool, resumeSpecialistSubAgentTool, resumeEphemeralSubAgent, accumulateStreamChunks)
import Lambda.Provider.Builtin (builtinToolsWithManager)
import Lambda.Provider.Mcp (McpClient, startAndLoadMcpServers, stopMcpClient)
import Lambda.Driver.OpenAI (openAiDynamicDriver)
import Lambda.Types

data EngineChannels = EngineChannels
  { cmdQueue :: !(TBQueue FrontendCommand)
  , evQueue  :: !(TQueue EngineEvent)
  }

initEngineChannels :: TQueue EngineEvent -> IO EngineChannels
initEngineChannels eQueue = do
  cQueue <- newTBQueueIO 64
  pure $ EngineChannels cQueue eQueue

-- | Helper to non-destructively update BLOCKED_ON in the State Vector and notify UI
updateBlockedOn :: AppEngineState -> Text -> IO ()
updateBlockedOn AppEngineState{..} status = atomically $ do
  curVec <- readTVar appStateVector
  let linesVec = T.lines curVec
      hasBlocked = any ("BLOCKED_ON:" `T.isPrefixOf`) linesVec
      updatedVec = if hasBlocked
        then T.unlines $ map (\l ->
          if "BLOCKED_ON:" `T.isPrefixOf` l
            then "BLOCKED_ON: " <> status
            else l) linesVec
        else curVec <> "\nBLOCKED_ON: " <> status
  writeTVar appStateVector updatedVec
  writeTQueue appEventQueue (EvWorkingStateUpdate updatedVec)

-- | First-class lifecycle container for the autonomous agent engine and background workers
data EngineRuntime = EngineRuntime
  { erEngineState :: !AppEngineState
  , erChannels    :: !EngineChannels
  , erWorkerAsync :: !(Async ())
  , erPromptAsync :: !(Async ())
  , erMcpClients  :: ![McpClient]
  }

-- | Bootstraps and starts a complete autonomous engine runtime with background tasks
startEngineRuntime :: Config -> Maybe Session -> Maybe ModelDriver -> IO EngineRuntime
startEngineRuntime cfg maybeSess mOverrideDriver = do
  httpMgr <- newTlsManager
  let builtinReg = registerTools (builtinToolsWithManager (Just httpMgr) (workspaceRoot cfg) (artifactDir cfg)) emptyRegistry
  (mcpClients, mcpTools) <- startAndLoadMcpServers (mcpServers cfg)
  let baseRegistry = registerTools mcpTools builtinReg

  secState <- initSecurityWithRoot (workspaceRoot cfg) (alwaysAllowGlobs cfg) (alwaysDenyGlobs cfg)
  engineState <- initEngineStateWithSession cfg baseRegistry secState maybeSess

  driver <- case mOverrideDriver of
    Just d  -> pure d
    Nothing -> openAiDynamicDriver cfg (readTVarIO (appActiveModel engineState))

  let fullRegistry = registerTool (resumeSpecialistSubAgentTool engineState driver)
                   $ registerTool (spawnSpecialistSubAgentTool engineState driver) baseRegistry
  atomically $ writeTVar (appToolRegistry engineState) fullRegistry

  channels <- initEngineChannels (appEventQueue engineState)
  activeTaskVar <- newTVarIO Nothing
  workerAsync <- async $ engineWorkerLoop engineState driver channels activeTaskVar
  promptAsync <- async $ forever $ do
    prompt <- atomically $ readTBQueue (uiPromptQueue (appSecurity engineState))
    emitEngineEvent engineState (EvPermissionRequired prompt)

  pure EngineRuntime
    { erEngineState = engineState
    , erChannels    = channels
    , erWorkerAsync = workerAsync
    , erPromptAsync = promptAsync
    , erMcpClients  = mcpClients
    }

-- | Safely shuts down background workers and spawned MCP server child processes
stopEngineRuntime :: EngineRuntime -> IO ()
stopEngineRuntime EngineRuntime{..} = do
  cancel erWorkerAsync
  cancel erPromptAsync
  mapM_ stopMcpClient erMcpClients

-- | Starts the headless autonomous agent loop in a background thread
startEngineLoop :: AppEngineState -> ModelDriver -> EngineChannels -> IO ()
startEngineLoop state driver channels = do
  activeTaskVar <- newTVarIO Nothing
  _ <- forkIO $ engineWorkerLoop state driver channels activeTaskVar
  _ <- forkIO $ forever $ do
    prompt <- atomically $ readTBQueue (uiPromptQueue (appSecurity state))
    emitEngineEvent state (EvPermissionRequired prompt)
  pure ()

engineWorkerLoop :: AppEngineState -> ModelDriver -> EngineChannels -> TVar (Maybe (Async ())) -> IO ()
engineWorkerLoop engineState@AppEngineState{..} driver channels@EngineChannels{..} activeTaskVar = do
  cmd <- atomically $ readTBQueue cmdQueue
  case cmd of
    CmdQuit -> do
      mTask <- readTVarIO activeTaskVar
      case mTask of
        Just t  -> cancel t
        Nothing -> pure ()
    CmdInterrupt -> do
      atomically $ writeTVar appInterrupted True
      mTask <- readTVarIO activeTaskVar
      case mTask of
        Just t  -> cancel t
        Nothing -> pure ()
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdSetMode newMode -> do
      atomically $ writeTVar appMode newMode
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdSetModel rawModel -> do
      let targetModel = resolveModelWithConfig appConfig rawModel
          targetLimit = lookupModelContextLimit targetModel
      atomically $ do
        writeTVar appActiveModel targetModel
        writeTVar appContextLimit targetLimit
      currentTurns <- readTVarIO appTurns
      let currentTokens = estimateTotalTokens currentTurns
      when (currentTokens > (targetLimit * 9 `div` 10)) $ do
        atomically $ do
          let compacted = compactHistory 0 4 currentTurns
          writeTVar appTurns compacted
        persistCurrentSession engineState
        updateBlockedOn engineState "Compacting context..."
      persistCurrentSession engineState
      emitEngineEvent engineState (EvModelSwitched targetModel targetLimit)
      _ <- addTurn engineState SystemRole [TextBlock ("Switched active model to " <> targetModel <> " (context limit: " <> T.pack (show targetLimit) <> " tokens)")]
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdCancelSubAgent sId -> do
      updateSubAgentStatus engineState sId (SubAgentBlocked "Cancelled by user")
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdResumeSubAgent sId extraBud mGuidance mRole -> do
      _ <- async $ do
        res <- resumeEphemeralSubAgent engineState driver sId extraBud mRole mGuidance []
        case res of
          Left err -> emitEngineEvent engineState (EvError err)
          Right _  -> pure ()
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdSetGoal newGoal -> do
      atomically $ do
        curVec <- readTVar appStateVector
        let linesVec = T.lines curVec
            hasGoal = any ("GOAL:" `T.isPrefixOf`) linesVec
            updatedVec = if hasGoal
              then T.unlines $ map (\l ->
                if "GOAL:" `T.isPrefixOf` l
                  then "GOAL: " <> newGoal
                  else l) linesVec
              else "GOAL: " <> newGoal <> "\n" <> curVec
        writeTVar appStateVector updatedVec
        writeTQueue appEventQueue (EvWorkingStateUpdate updatedVec)
      persistCurrentSession engineState
      _ <- addTurn engineState SystemRole [TextBlock ("Goal set to: " <> newGoal)]
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdCompactHistory -> do
      -- Real context compaction trigger
      atomically $ do
        ts <- readTVar appTurns
        let compacted = compactHistory 0 4 ts
        writeTVar appTurns compacted
      persistCurrentSession engineState
      updateBlockedOn engineState "Context compaction completed"
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdSystemMessage msg -> do
      _ <- addTurn engineState SystemRole [TextBlock msg]
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdClearHistory -> do
      atomically $ do
        writeTVar appTurns []
        writeTVar appTurnCounter 1
      _ <- addTurn engineState SystemRole [TextBlock "Conversation history cleared."]
      persistCurrentSession engineState
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdNewSession -> do
      _ <- resetEngineSession engineState
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdRewindTurns n -> do
      mRewound <- atomically $ do
        ts <- readTVar appTurns
        if length ts <= 1
          then pure Nothing
          else do
            let dropCount = max 1 (n * 2)
                remaining = take (max 1 (length ts - dropCount)) ts
                popped = drop (length remaining) ts
            modifyTVar' appUndoStack (\s -> popped : s)
            writeTVar appTurns remaining
            subs <- readTVar appSubAgents
            let updatedSubs = Map.map (\t -> if subAgentStatus t == SubAgentRunning then t { subAgentStatus = SubAgentBlocked "Cancelled on session rewind" } else t) subs
            writeTVar appSubAgents updatedSubs
            pure (Just remaining)
      case mRewound of
        Nothing -> updateBlockedOn engineState "Cannot rewind: at start of session"
        Just _ -> do
          persistCurrentSession engineState
          updateBlockedOn engineState ("Rewound " <> T.pack (show n) <> " turn(s)")
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdForkSession mTitle -> do
      currSess <- snapshotSession engineState
      childSess <- forkSession appSessionDir currSess mTitle
      restoreEngineSession engineState childSess
      persistCurrentSession engineState
      emitEngineEvent engineState (EvSessionSwitched (sessionId childSess) (sessionMode childSess) (sessionTurns childSess) (sessionSubAgents childSess))
      updateBlockedOn engineState ("Forked into " <> sessionId childSess)
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdSwitchSession sid -> do
      loadRes <- loadSession appSessionDir sid
      case loadRes of
        Left err ->
          emitEngineEvent engineState (EvError $ "Failed to switch session: " <> err)
        Right sess ->
          restoreEngineSession engineState sess
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdExportTrace -> do
      snap <- snapshotSession engineState
      path <- exportSessionTrace appSessionDir snap
      _ <- addTurn engineState SystemRole [TextBlock $ "Debug execution trace exported to: " <> T.pack path]
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdUserPrompt promptText -> do
      -- If prior turn task is still active, cancel it before running new prompt
      mPrev <- readTVarIO activeTaskVar
      case mPrev of
        Just prev -> cancel prev
        Nothing   -> pure ()

      -- 1. Reset interrupt flag, update State Vector, and add user turn
      atomically $ do
        writeTVar appInterrupted False
        curVec <- readTVar appStateVector
        let linesVec = T.lines curVec
            hasGoal = any ("GOAL:" `T.isPrefixOf`) linesVec
            currentGoal = case filter ("GOAL:" `T.isPrefixOf`) linesVec of
              (g:_) -> T.strip (T.drop 5 g)
              []    -> "Awaiting task"
            newGoal
              | "/goal " `T.isPrefixOf` promptText = T.strip (T.drop 6 promptText)
              | currentGoal == "Awaiting task" || T.null currentGoal = deriveTitle promptText
              | otherwise = currentGoal
            updatedVec = if hasGoal
              then T.unlines $ map (\l ->
                case () of
                  _ | "GOAL:" `T.isPrefixOf` l -> "GOAL: " <> newGoal
                  _ | "BLOCKED_ON:" `T.isPrefixOf` l -> "BLOCKED_ON: Model execution"
                  _ -> l) linesVec
              else "GOAL: " <> newGoal
                <> "\nINVARIANTS: [Safe workspace ops, User grant required]"
                <> "\nACTIVE_HYPOTHESIS: Formulating execution path"
                <> "\nBLOCKED_ON: Model execution"
        writeTVar appStateVector updatedVec
        writeTQueue appEventQueue (EvWorkingStateUpdate updatedVec)

      _ <- addTurn engineState UserRole [TextBlock promptText]

      -- Check if automatic context compaction is required prior to LLM call
      atomically $ do
        ts <- readTVar appTurns
        let maxLimit = contextWindowLimit appConfig
        if estimateTotalTokens ts > maxLimit
          then writeTVar appTurns (compactHistory maxLimit 4 ts)
          else pure ()

      -- 2. Run agent conversation loop in async task so cmdQueue remains responsive
      turnTask <- async $ do
        turnRes <- try $ runAgentTurnLoop engineState driver channels 1 15
        case turnRes of
          Left (ex :: SomeException) -> do
            intr <- readTVarIO appInterrupted
            unless intr $
              emitEngineEvent engineState (EvError $ "Engine loop failure: " <> T.pack (show ex))
          Right () -> pure ()

        atomically $ do
          curVec <- readTVar appStateVector
          let updatedVec = T.unlines $ map (\l -> if "BLOCKED_ON:" `T.isPrefixOf` l then "BLOCKED_ON: User input" else l) (T.lines curVec)
          writeTVar appStateVector updatedVec
          writeTQueue appEventQueue (EvWorkingStateUpdate updatedVec)
          writeTVar activeTaskVar Nothing

        -- Auto-save session state after turn completion
        persistCurrentSession engineState
        emitEngineEvent engineState EvDone

      atomically $ writeTVar activeTaskVar (Just turnTask)
      engineWorkerLoop engineState driver channels activeTaskVar

runAgentTurnLoop
  :: AppEngineState
  -> ModelDriver
  -> EngineChannels
  -> Int
  -> Int
  -> IO ()
runAgentTurnLoop engineState@AppEngineState{..} driver channels@EngineChannels{..} turnIdx maxTurns
  | turnIdx > maxTurns = do
      emitEngineEvent engineState (EvError "Maximum turn limit reached for current prompt.")
  | otherwise = do
      turns <- readTVarIO appTurns
      mode <- readTVarIO appMode
      reg <- readTVarIO appToolRegistry
      let toolSchemas = toolsToOpenAISchema mode reg

      -- Add in-flight Assistant turn immediately for instant user feedback
      asstTurn <- addTurn engineState AssistantRole [TextBlock "Thinking..."]

      let onProgress t th = do
            let blks = [ ThinkingBlock turnIdx th Visible | not (T.null th) ]
                    ++ [ TextBlock t | not (T.null t) ]
            updateTurnBlocks engineState (turnId asstTurn) blks
          onChunk chunk =
            atomically $ writeTQueue evQueue (EvStreamChunk chunk)

      (fullText, fullThinking, toolCalls, wasInterrupted, _mDriverErr) <-
        accumulateStreamChunks appInterrupted driver turns toolSchemas onProgress onChunk

      if wasInterrupted
        then do
          let intBlocks =
                [ ThinkingBlock turnIdx fullThinking Visible | not (T.null fullThinking) ]
                ++ [ TextBlock (fullText <> (if T.null fullText then "" else "\n") <> "⚠️ [Turn interrupted by user]") ]
          updateTurnBlocks engineState (turnId asstTurn) intBlocks
        else do
          let assistantBlocks =
                [ ThinkingBlock turnIdx fullThinking Visible | not (T.null fullThinking) ]
                ++ [ TextBlock fullText | not (T.null fullText) ]
                ++ map ToolCallBlock toolCalls

          -- Finalize assistant turn with complete text and tool calls
          updateTurnBlocks engineState (turnId asstTurn) assistantBlocks

          -- If tool calls were generated, execute them and loop
          if null toolCalls
            then pure ()
            else do
              atomically $ do
                curVec <- readTVar appStateVector
                let toolNames = T.intercalate ", " (map toolCallName toolCalls)
                    updatedVec = T.unlines $ map (\l ->
                      if "BLOCKED_ON:" `T.isPrefixOf` l
                        then "BLOCKED_ON: Running " <> toolNames
                        else l) (T.lines curVec)
                writeTVar appStateVector updatedVec
                writeTQueue appEventQueue (EvWorkingStateUpdate updatedVec)

              results <- runToolsWithInterrupt engineState toolCalls
              _ <- addTurn engineState ToolRole (map ToolResultBlock results)
              stillActive <- not <$> readTVarIO appInterrupted
              when stillActive $
                runAgentTurnLoop engineState driver channels (turnIdx + 1) maxTurns
  where
    runToolsWithInterrupt es tcs = do
      intr <- readTVarIO appInterrupted
      if intr
        then pure [ToolResult (toolCallId tc) "" "Execution cancelled by user." Nothing | tc <- tcs]
        else mapConcurrently (runSingleToolWithInterrupt es) tcs

    runSingleToolWithInterrupt es tc = do
      res <- race
        (atomically $ do
          intr <- readTVar appInterrupted
          check intr)
        (executeToolDispatch es MainAgent [] tc)
      case res of
        Left () -> pure $ ToolResult (toolCallId tc) "" "Execution cancelled by user." Nothing
        Right r -> pure r
