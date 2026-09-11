{-# LANGUAGE OverloadedStrings #-}

module Lambda.UI.Types where

import Brick.Widgets.Edit (Editor)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Lambda.Core.EngineInterface (EngineChannels)
import Lambda.Types

data ResourceName
  = ChatView
  | ThinkingFold !Int
  | SubAgentView
  | SubAgentItem !Int
  | EditorInput
  | ButtonAlways
  | ButtonOnce
  | ButtonNo
  | ButtonNever
  | HudOverlay
  | HudScroll
  | CompletionPopup
  deriving (Eq, Ord, Show)

data Candidate = Candidate
  { candInsert  :: !Text
  , candDisplay :: !Text
  } deriving (Eq, Show)

simpleCandidate :: Text -> Candidate
simpleCandidate t = Candidate t t

data CompletionState = CompletionState
  { compCandidates :: ![Candidate]
  , compSelected   :: !Int
  } deriving (Eq, Show)

compMatches :: CompletionState -> [Text]
compMatches = map candInsert . compCandidates

data UIState = UIState
  { uiTurns            :: ![Turn]
  , uiSubAgents        :: !(Map Int SubAgentTask)
  , uiCurrentPrompt    :: !(Maybe PermissionPrompt)
  , uiPendingPrompts   :: !(Seq PermissionPrompt)
  , uiMode             :: !AgentMode
  , uiEditor           :: !(Editor Text ResourceName)
  , uiWorkingState     :: !Text
  , uiChannels         :: !EngineChannels
  , uiLastEscTime      :: !(Maybe UTCTime)
  , uiContextLimit     :: !Int
  , uiPromptHistory    :: ![Text]
  , uiHistoryIndex     :: !(Maybe Int)
  , uiSavedDraft       :: !Text
  , uiModelName        :: !Text
  , uiThinkingVisible  :: !Bool
  , uiSelectedSubAgent :: !(Maybe Int)
  , uiShowHud          :: !Bool
  , uiCompletion       :: !(Maybe CompletionState)
  , uiIsGenerating     :: !Bool
  }
