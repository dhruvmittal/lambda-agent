{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Core.EngineInterface
  ( EngineChannels(..)
  , initEngineChannels
  , startEngineLoop
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider (toolsToOpenAISchema)
import Lambda.Engine.Dispatcher (executeToolDispatch)
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
    CmdUserPrompt promptText -> do
      -- 1. Add user turn
      _ <- addTurn engineState UserRole [TextBlock promptText]

      -- 2. Run agent conversation loop with exception protection
      turnRes <- try $ runAgentTurnLoop engineState driver EngineChannels{..} 1 15
      case turnRes of
        Left (ex :: SomeException) ->
          emitEngineEvent engineState (EvError $ "Engine loop failure: " <> T.pack (show ex))
        Right () -> pure ()
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
        atomically $ writeTQueue evQueue (EvStreamChunk chunk)
        case chunk of
          ChunkText txt -> do
            atomically $ modifyTVar' accumTextVar (<> txt)
            t <- readTVarIO accumTextVar
            th <- readTVarIO accumThinkingVar
            let blks = [ ThinkingBlock turnIdx th Collapsed | not (T.null th) ]
                    ++ [ TextBlock t ]
            updateTurnBlocks engineState (turnId asstTurn) blks
          ChunkThinking th -> do
            atomically $ modifyTVar' accumThinkingVar (<> th)
            t <- readTVarIO accumTextVar
            th' <- readTVarIO accumThinkingVar
            let blks = [ ThinkingBlock turnIdx th' Collapsed ]
                    ++ [ TextBlock t | not (T.null t) ]
            updateTurnBlocks engineState (turnId asstTurn) blks
          ChunkToolCallStart cid name -> atomically $
            modifyTVar' toolCallsVar (\tcs -> tcs ++ [ToolCall cid name (Aeson.object [])])
          ChunkToolCallArgs cid argsChunk -> atomically $
            modifyTVar' toolCallsVar (\tcs -> map (appendArgs cid argsChunk) tcs)
          ChunkDone -> pure ()

      fullText <- readTVarIO accumTextVar
      fullThinking <- readTVarIO accumThinkingVar
      toolCalls <- readTVarIO toolCallsVar

      let assistantBlocks =
            [ ThinkingBlock turnIdx fullThinking Collapsed | not (T.null fullThinking) ]
            ++ [ TextBlock fullText | not (T.null fullText) ]
            ++ map ToolCallBlock toolCalls

      -- Finalize assistant turn with complete text and tool calls
      updateTurnBlocks engineState (turnId asstTurn) assistantBlocks

      -- If tool calls were generated, execute them and loop
      if null toolCalls
        then pure ()
        else do
          results <- mapM (executeToolDispatch engineState MainAgent []) toolCalls
          _ <- addTurn engineState ToolRole (map ToolResultBlock results)
          runAgentTurnLoop engineState driver channels (turnIdx + 1) maxTurns
  where
    appendArgs targetId chunk tc@ToolCall{..}
      | toolCallId == targetId =
          let existing = case toolCallArgs of
                Aeson.String s -> s
                _              -> ""
          in tc { toolCallArgs = Aeson.String (existing <> chunk) }
      | otherwise = tc
