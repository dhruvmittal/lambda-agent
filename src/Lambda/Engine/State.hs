{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.State
  ( AppEngineState(..)
  , initEngineState
  , initEngineStateWithSession
  , addTurn
  , updateTurnBlocks
  , registerSubAgentTask
  , updateSubAgentStatus
  , updateSubAgentTurns
  , emitEngineEvent
  , snapshotSession
  , persistCurrentSession
  , resetEngineSession
  , restoreEngineSession
  ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import System.FilePath ((</>))

import Lambda.Config (Config(..))
import Lambda.Core.ToolProvider (ToolRegistry)
import Lambda.Engine.Security (SecurityState)
import Lambda.Engine.Session
  ( Session(..)
  , deriveTitle
  , newSession
  , saveSession
  )
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
  , appSession      :: !(TVar Session)
  , appSessionDir   :: !FilePath
  }

initEngineState :: Config -> ToolRegistry -> SecurityState -> IO AppEngineState
initEngineState cfg reg sec = initEngineStateWithSession cfg reg sec Nothing

initEngineStateWithSession :: Config -> ToolRegistry -> SecurityState -> Maybe Session -> IO AppEngineState
initEngineStateWithSession cfg reg sec maybeSess = do
  let sessDir = workspaceRoot cfg </> ".lambda" </> "sessions"
  sess <- case maybeSess of
    Just s  -> pure s
    Nothing -> newSession
  modeVar   <- newTVarIO (sessionMode sess)
  let initialTurns = if null (sessionTurns sess)
        then [Turn 1 SystemRole [TextBlock "lambdA initialized. Enter a goal or press /help for commands."]]
        else sessionTurns sess
      nextTurnId = if null initialTurns then 2 else maximum (map turnId initialTurns) + 1
      initialSubs = sessionSubAgents sess
      nextSubSeq = if Map.null initialSubs then 1 else maximum (Map.keys initialSubs) + 1
      initialStVec = case Map.lookup "state_vector" (sessionStateVector sess) of
        Just sv -> sv
        Nothing -> "GOAL: Awaiting task\nINVARIANTS: [Safe workspace ops, User grant required]\nACTIVE_HYPOTHESIS: None\nBLOCKED_ON: User input"
  turnsVar  <- newTVarIO initialTurns
  tCountVar <- newTVarIO nextTurnId
  subsVar   <- newTVarIO initialSubs
  subSeqVar <- newTVarIO nextSubSeq
  stVecVar  <- newTVarIO initialStVec
  regVar    <- newTVarIO reg
  evQueue   <- newTQueueIO
  intrVar   <- newTVarIO False
  sessVar   <- newTVarIO sess
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
    , appSession      = sessVar
    , appSessionDir   = sessDir
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

registerSubAgentTask :: AppEngineState -> Text -> Text -> Int -> IO SubAgentTask
registerSubAgentTask AppEngineState{..} role hypothesis budget = atomically $ do
  sId <- readTVar appSubAgentSeq
  writeTVar appSubAgentSeq (sId + 1)
  let task = SubAgentTask sId role hypothesis 0 budget SubAgentRunning Nothing []
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

updateSubAgentTurns :: AppEngineState -> Int -> [Turn] -> IO ()
updateSubAgentTurns AppEngineState{..} sId turns = atomically $ do
  subs <- readTVar appSubAgents
  case Map.lookup sId subs of
    Just task -> do
      let asstCount = length (filter (\t -> turnRole t == AssistantRole) turns)
          updated = task { subAgentTurns = turns, subAgentTurnCount = asstCount }
      writeTVar appSubAgents (Map.insert sId updated subs)
      writeTQueue appEventQueue (EvSubAgentUpdate updated)
    Nothing -> pure ()

emitEngineEvent :: AppEngineState -> EngineEvent -> IO ()
emitEngineEvent AppEngineState{..} ev = atomically $ writeTQueue appEventQueue ev

snapshotSession :: AppEngineState -> IO Session
snapshotSession AppEngineState{..} = do
  now <- getCurrentTime
  atomically $ do
    currSess <- readTVar appSession
    curMode  <- readTVar appMode
    curTurns <- readTVar appTurns
    curSubs  <- readTVar appSubAgents
    curStVec <- readTVar appStateVector
    let newTitle = if sessionTitle currSess == "New Session"
          then case [t | Turn _ UserRole blocks <- curTurns, TextBlock t <- blocks] of
                 (firstPrompt:_) -> deriveTitle firstPrompt
                 []              -> sessionTitle currSess
          else sessionTitle currSess
    let snap = currSess
          { sessionUpdatedAt     = now
          , sessionTitle         = newTitle
          , sessionMode          = curMode
          , sessionTurns         = curTurns
          , sessionSubAgents     = curSubs
          , sessionStateVector   = Map.singleton "state_vector" curStVec
          }
    writeTVar appSession snap
    pure snap

persistCurrentSession :: AppEngineState -> IO ()
persistCurrentSession state@AppEngineState{..} = do
  snap <- snapshotSession state
  saveSession appSessionDir (maxSavedSessions appConfig) snap

resetEngineSession :: AppEngineState -> IO Session
resetEngineSession state@AppEngineState{..} = do
  persistCurrentSession state
  freshSess <- newSession
  atomically $ do
    writeTVar appSession freshSess
    writeTVar appMode PlanMode
    let initTurns = [Turn 1 SystemRole [TextBlock "New session started. Enter a goal or press /help for commands."]]
    writeTVar appTurns initTurns
    writeTVar appTurnCounter 2
    writeTVar appSubAgents Map.empty
    writeTVar appSubAgentSeq 1
    writeTVar appStateVector "GOAL: Awaiting task\nINVARIANTS: [Safe workspace ops, User grant required]\nACTIVE_HYPOTHESIS: None\nBLOCKED_ON: User input"
    writeTQueue appEventQueue (EvSessionSwitched (sessionId freshSess) PlanMode initTurns Map.empty)
  pure freshSess

restoreEngineSession :: AppEngineState -> Session -> IO ()
restoreEngineSession state@AppEngineState{..} s = do
  persistCurrentSession state
  atomically $ do
    writeTVar appSession s
    writeTVar appMode (sessionMode s)
    let turns = if null (sessionTurns s)
          then [Turn 1 SystemRole [TextBlock "Resumed session. Enter a goal or press /help for commands."]]
          else sessionTurns s
        nextTurnId = if null turns then 2 else maximum (map turnId turns) + 1
        subs = sessionSubAgents s
        nextSubId = if Map.null subs then 1 else maximum (Map.keys subs) + 1
        stVec = case Map.lookup "state_vector" (sessionStateVector s) of
          Just sv -> sv
          Nothing -> "GOAL: Awaiting task\nINVARIANTS: [Safe workspace ops, User grant required]\nACTIVE_HYPOTHESIS: None\nBLOCKED_ON: User input"
    writeTVar appTurns turns
    writeTVar appTurnCounter nextTurnId
    writeTVar appSubAgents subs
    writeTVar appSubAgentSeq nextSubId
    writeTVar appStateVector stVec
    writeTQueue appEventQueue (EvSessionSwitched (sessionId s) (sessionMode s) turns subs)
