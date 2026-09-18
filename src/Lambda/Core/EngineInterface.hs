{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Core.EngineInterface
  ( EngineChannels(..)
  , initEngineChannels
  , startEngineLoop
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently, race)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forever, when, unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

import qualified Data.Map.Strict as Map
import Lambda.Config (Config(..), resolveModelWithConfig, lookupModelContextLimit)
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider (toolsToOpenAISchema)
import Lambda.Engine.Compactor (compactHistory, estimateTotalTokens)
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.Security (SecurityState(..))
import Lambda.Engine.Session (exportSessionTrace, loadSession, forkSession, Session(..))
import Lambda.Engine.State
import Lambda.Types

data EngineChannels = EngineChannels
  { cmdQueue :: !(TBQueue FrontendCommand)
  , evQueue  :: !(TQueue EngineEvent)
  }

initEngineChannels :: TQueue EngineEvent -> IO EngineChannels
initEngineChannels eQueue = do
  cQueue <- newTBQueueIO 64
  pure $ EngineChannels cQueue eQueue

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
        emitEngineEvent engineState (EvWorkingStateUpdate "Context compaction auto-triggered on model switch.")
      persistCurrentSession engineState
      emitEngineEvent engineState (EvModelSwitched targetModel targetLimit)
      _ <- addTurn engineState SystemRole [TextBlock ("Switched active model to " <> targetModel <> " (context limit: " <> T.pack (show targetLimit) <> " tokens)")]
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdCancelSubAgent sId -> do
      updateSubAgentStatus engineState sId (SubAgentBlocked "Cancelled by user")
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdCompactHistory -> do
      -- Real context compaction trigger
      atomically $ do
        ts <- readTVar appTurns
        let compacted = compactHistory 0 4 ts
        writeTVar appTurns compacted
      persistCurrentSession engineState
      emitEngineEvent engineState (EvWorkingStateUpdate "Context compaction completed.")
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
      ts <- readTVarIO appTurns
      let dropCount = max 1 (n * 2)
      if length ts <= 1
        then emitEngineEvent engineState (EvWorkingStateUpdate "Cannot rewind: already at start of session.")
        else do
          let remaining = take (max 1 (length ts - dropCount)) ts
              popped = drop (length remaining) ts
          atomically $ do
            modifyTVar' appUndoStack (\s -> popped : s)
            writeTVar appTurns remaining
            subs <- readTVar appSubAgents
            let updatedSubs = Map.map (\t -> if subAgentStatus t == SubAgentRunning then t { subAgentStatus = SubAgentBlocked "Cancelled on session rewind" } else t) subs
            writeTVar appSubAgents updatedSubs
          persistCurrentSession engineState
          emitEngineEvent engineState (EvWorkingStateUpdate $ "Rewound " <> T.pack (show n) <> " turn(s). Previous turns saved to undo stack.")
      engineWorkerLoop engineState driver channels activeTaskVar
    CmdForkSession mTitle -> do
      currSess <- snapshotSession engineState
      childSess <- forkSession appSessionDir currSess mTitle
      restoreEngineSession engineState childSess
      persistCurrentSession engineState
      emitEngineEvent engineState (EvSessionSwitched (sessionId childSess) (sessionMode childSess) (sessionTurns childSess) (sessionSubAgents childSess))
      emitEngineEvent engineState (EvWorkingStateUpdate $ "Forked into session " <> sessionId childSess)
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
      let goalText = if "/goal " `T.isPrefixOf` promptText
                       then T.strip (T.drop 6 promptText)
                       else promptText
      atomically $ do
        writeTVar appInterrupted False
        let newVec = "GOAL: " <> goalText
                  <> "\nINVARIANTS: [Safe workspace ops, User grant required]"
                  <> "\nACTIVE_HYPOTHESIS: Formulating execution path"
                  <> "\nBLOCKED_ON: Model execution"
        writeTVar appStateVector newVec
        writeTQueue appEventQueue (EvWorkingStateUpdate newVec)

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

      accumTextVar <- newTVarIO ("" :: Text)
      accumThinkingVar <- newTVarIO ("" :: Text)
      toolCallsVar <- newTVarIO ([] :: [ToolCall])

      -- Add in-flight Assistant turn immediately for instant user feedback
      asstTurn <- addTurn engineState AssistantRole [TextBlock "Thinking..."]

      -- Stream LLM response
      streamCompletion driver turns toolSchemas $ \chunk -> do
        intr <- readTVarIO appInterrupted
        unless intr $ do
          atomically $ writeTQueue evQueue (EvStreamChunk chunk)
          case chunk of
            ChunkText txt -> do
              atomically $ modifyTVar' accumTextVar (<> txt)
              t <- readTVarIO accumTextVar
              th <- readTVarIO accumThinkingVar
              let blks = [ ThinkingBlock turnIdx th Visible | not (T.null th) ]
                      ++ [ TextBlock t ]
              updateTurnBlocks engineState (turnId asstTurn) blks
            ChunkThinking th -> do
              atomically $ modifyTVar' accumThinkingVar (<> th)
              t <- readTVarIO accumTextVar
              th' <- readTVarIO accumThinkingVar
              let blks = [ ThinkingBlock turnIdx th' Visible ]
                      ++ [ TextBlock t | not (T.null t) ]
              updateTurnBlocks engineState (turnId asstTurn) blks
            ChunkToolCallStart cid name -> atomically $
              modifyTVar' toolCallsVar (\tcs -> tcs ++ [ToolCall cid name (Aeson.String "")])
            ChunkToolCallArgs cid argsChunk -> atomically $
              modifyTVar' toolCallsVar (appendArgs cid argsChunk)
            ChunkDone -> pure ()

      fullText <- readTVarIO accumTextVar
      fullThinking <- readTVarIO accumThinkingVar
      rawToolCalls <- readTVarIO toolCallsVar
      let toolCalls = map finalizeToolArgs rawToolCalls
      wasInterrupted <- readTVarIO appInterrupted

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

    appendArgs targetId chunk tcs
      | T.null targetId =
          if null tcs
            then []
            else let (prev, lastTc) = (init tcs, last tcs)
                 in prev ++ [addChunk chunk lastTc]
      | "idx_" `T.isPrefixOf` targetId =
          case (readMaybe (T.unpack (T.drop 4 targetId)) :: Maybe Int) of
            Just idx | idx >= 0 && idx < length tcs ->
              case splitAt idx tcs of
                (before, target : after) -> before ++ [addChunk chunk target] ++ after
                _ -> map (\tc -> if toolCallId tc == targetId then addChunk chunk tc else tc) tcs
            _ -> map (\tc -> if toolCallId tc == targetId then addChunk chunk tc else tc) tcs
      | otherwise =
          map (\tc -> if toolCallId tc == targetId then addChunk chunk tc else tc) tcs
      where
        addChunk ch tc =
          let existing = case toolCallArgs tc of
                Aeson.String s -> s
                _              -> ""
          in tc { toolCallArgs = Aeson.String (existing <> ch) }

    finalizeToolArgs tc@ToolCall{..} =
      case toolCallArgs of
        Aeson.String s
          | T.null (T.strip s) -> tc { toolCallArgs = Aeson.object [] }
          | otherwise ->
              case Aeson.decode (BL.fromStrict $ TE.encodeUtf8 s) of
                Just val -> tc { toolCallArgs = val }
                Nothing  -> tc { toolCallArgs = Aeson.object ["raw" .= s] }
        _ -> tc
