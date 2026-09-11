{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Core.EngineInterface
  ( EngineChannels(..)
  , initEngineChannels
  , startEngineLoop
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forever, when, unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider (toolsToOpenAISchema)
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.Security (SecurityState(..))
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
  _ <- forkIO $ engineWorkerLoop state driver channels
  _ <- forkIO $ forever $ do
    prompt <- atomically $ readTBQueue (uiPromptQueue (appSecurity state))
    emitEngineEvent state (EvPermissionRequired prompt)
  pure ()

engineWorkerLoop :: AppEngineState -> ModelDriver -> EngineChannels -> IO ()
engineWorkerLoop engineState@AppEngineState{..} driver EngineChannels{..} = do
  cmd <- atomically $ readTBQueue cmdQueue
  case cmd of
    CmdQuit -> pure ()
    CmdSetMode newMode -> do
      atomically $ writeTVar appMode newMode
      engineWorkerLoop engineState driver EngineChannels{..}
    CmdResolvePermission _ _ -> do
      -- Handled directly via TMVar resolution in security prompt
      engineWorkerLoop engineState driver EngineChannels{..}
    CmdCancelSubAgent sId -> do
      updateSubAgentStatus engineState sId (SubAgentBlocked "Cancelled by user")
      engineWorkerLoop engineState driver EngineChannels{..}
    CmdCompactHistory -> do
      -- Context compaction trigger
      ts <- readTVarIO appTurns
      let summaryText = "[Compaction Checkpoint: " <> T.pack (show (length ts)) <> " historical turns preserved in disk archives]"
      _ <- addTurn engineState SystemRole [TextBlock summaryText]
      engineWorkerLoop engineState driver EngineChannels{..}
    CmdInterrupt -> do
      atomically $ writeTVar appInterrupted True
      engineWorkerLoop engineState driver EngineChannels{..}
    CmdSystemMessage msg -> do
      _ <- addTurn engineState SystemRole [TextBlock msg]
      engineWorkerLoop engineState driver EngineChannels{..}
    CmdClearHistory -> do
      atomically $ do
        writeTVar appTurns []
        writeTVar appTurnCounter 1
      _ <- addTurn engineState SystemRole [TextBlock "Conversation history cleared."]
      engineWorkerLoop engineState driver EngineChannels{..}
    CmdUserPrompt promptText -> do
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

      -- 2. Run agent conversation loop with exception protection
      turnRes <- try $ runAgentTurnLoop engineState driver EngineChannels{..} 1 15
      case turnRes of
        Left (ex :: SomeException) ->
          emitEngineEvent engineState (EvError $ "Engine loop failure: " <> T.pack (show ex))
        Right () -> pure ()

      atomically $ do
        curVec <- readTVar appStateVector
        let updatedVec = T.unlines $ map (\l -> if "BLOCKED_ON:" `T.isPrefixOf` l then "BLOCKED_ON: User input" else l) (T.lines curVec)
        writeTVar appStateVector updatedVec
        writeTQueue appEventQueue (EvWorkingStateUpdate updatedVec)

      engineWorkerLoop engineState driver EngineChannels{..}

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
