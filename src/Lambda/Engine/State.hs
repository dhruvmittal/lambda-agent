{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.State
  ( AppEngineState(..)
  , initEngineState
  , addTurn
  , updateTurnBlocks
  , registerSubAgentTask
  , updateSubAgentStatus
  , emitEngineEvent
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Lambda.Config (Config)
import Lambda.Core.ToolProvider (ToolRegistry)
import Lambda.Engine.Security (SecurityState)
import Lambda.Types

data AppEngineState = AppEngineState
  { appConfig       :: !Config
  , appMode         :: !(TVar AgentMode)
  , appTurns        :: !(TVar [Turn])
  , appTurnCounter  :: !(TVar Int)
  , appSubAgents    :: !(TVar (Map Int SubAgentTask))
  , appSubAgentSeq  :: !(TVar Int)
  , appStateVector  :: !(TVar Text)
  , appToolRegistry :: !(TVar ToolRegistry)
  , appSecurity     :: !SecurityState
  , appEventQueue   :: !(TQueue EngineEvent)
  , appInterrupted  :: !(TVar Bool)
  }

initEngineState :: Config -> ToolRegistry -> SecurityState -> IO AppEngineState
initEngineState cfg reg sec = do
  modeVar   <- newTVarIO PlanMode
  turnsVar  <- newTVarIO [Turn 1 SystemRole [TextBlock "lambdA initialized. Enter a goal or press /help for commands."]]
  tCountVar <- newTVarIO 2
  subsVar   <- newTVarIO Map.empty
  subSeqVar <- newTVarIO 1
  stVecVar  <- newTVarIO "GOAL: Awaiting task\nINVARIANTS: [Safe workspace ops, User grant required]\nACTIVE_HYPOTHESIS: None\nBLOCKED_ON: User input"
  regVar    <- newTVarIO reg
  evQueue   <- newTQueueIO
  intrVar   <- newTVarIO False
  pure AppEngineState
    { appConfig       = cfg
    , appMode         = modeVar
    , appTurns        = turnsVar
    , appTurnCounter  = tCountVar
    , appSubAgents    = subsVar
    , appSubAgentSeq  = subSeqVar
    , appStateVector  = stVecVar
    , appToolRegistry = regVar
    , appSecurity     = sec
    , appEventQueue   = evQueue
    , appInterrupted  = intrVar
    }

addTurn :: AppEngineState -> Role -> [ContentBlock] -> IO Turn
addTurn AppEngineState{..} role blocks = atomically $ do
  currId <- readTVar appTurnCounter
  writeTVar appTurnCounter (currId + 1)
  let newTurn = Turn currId role blocks
  modifyTVar' appTurns (\ts -> ts ++ [newTurn])
  writeTQueue appEventQueue (EvTurnAdded newTurn)
  pure newTurn

updateTurnBlocks :: AppEngineState -> Int -> [ContentBlock] -> IO ()
updateTurnBlocks AppEngineState{..} tId blocks = atomically $ do
  ts <- readTVar appTurns
  let updated = map (\t -> if turnId t == tId then t { turnBlocks = blocks } else t) ts
  writeTVar appTurns updated
  case filter (\t -> turnId t == tId) updated of
    (u:_) -> writeTQueue appEventQueue (EvTurnUpdated u)
    []    -> pure ()

registerSubAgentTask :: AppEngineState -> Text -> Int -> IO SubAgentTask
registerSubAgentTask AppEngineState{..} hypothesis budget = atomically $ do
  sId <- readTVar appSubAgentSeq
  writeTVar appSubAgentSeq (sId + 1)
  let task = SubAgentTask sId hypothesis 0 budget SubAgentRunning Nothing
  modifyTVar' appSubAgents (Map.insert sId task)
  writeTQueue appEventQueue (EvSubAgentUpdate task)
  pure task

updateSubAgentStatus :: AppEngineState -> Int -> SubAgentStatus -> IO ()
updateSubAgentStatus AppEngineState{..} sId status = atomically $ do
  subs <- readTVar appSubAgents
  case Map.lookup sId subs of
    Just task -> do
      let updated = task { subAgentStatus = status }
      writeTVar appSubAgents (Map.insert sId updated subs)
      writeTQueue appEventQueue (EvSubAgentUpdate updated)
    Nothing -> pure ()

emitEngineEvent :: AppEngineState -> EngineEvent -> IO ()
emitEngineEvent AppEngineState{..} ev = atomically $ writeTQueue appEventQueue ev
