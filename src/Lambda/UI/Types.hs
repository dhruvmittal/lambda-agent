{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambda.UI.Types where

import qualified Brick.Widgets.Edit as E
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Lens.Micro.TH (makeLensesFor)

import Lambda.Config (Config)
import Lambda.Core.EngineInterface (EngineChannels)
import Lambda.Engine.Session (SessionMeta)
import Lambda.Types

data ResourceName
  = ChatView
  | ThinkingFold !Int
  | SubAgentView
  | SubAgentItem !Int
  | EditorInput
  | ButtonAlways
  | ButtonSession
  | ButtonOnce
  | ButtonDeny
  | ButtonNo
  | ButtonNever
  | HudOverlay
  | HudScroll
  | CompletionPopup
  | SessionChooserOverlay
  | SessionItem !Int
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

data SessionChooserState = SessionChooserState
  { scSessions :: ![SessionMeta]
  , scSelected :: !Int
  , scActiveId :: !Text
  } deriving (Eq, Show)

data UIState = UIState
  { uiTurns            :: ![Turn]
  , uiSubAgents        :: !(Map Int SubAgentTask)
  , uiCurrentPrompt    :: !(Maybe PermissionPrompt)
  , uiPendingPrompts   :: !(Seq PermissionPrompt)
  , uiMode             :: !AgentMode
  , uiEditor           :: !(E.Editor Text ResourceName)
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
  , uiSessionChooser   :: !(Maybe SessionChooserState)
  , uiIsGenerating     :: !Bool
  , uiSpinnerStep      :: !Int
  , uiExecutionStart   :: !(Maybe UTCTime)
  , uiElapsedSeconds   :: !Int
  , uiConfig           :: !Config
  }

makeLensesFor
  [ ("uiTurns", "turnsL")
  , ("uiSubAgents", "subAgentsL")
  , ("uiWorkingState", "workingStateL")
  , ("uiMode", "modeL")
  , ("uiThinkingVisible", "thinkingVisibleL")
  , ("uiIsGenerating", "isGeneratingL")
  , ("uiSpinnerStep", "spinnerStepL")
  , ("uiExecutionStart", "executionStartL")
  , ("uiElapsedSeconds", "elapsedSecondsL")
  , ("uiModelName", "modelNameL")
  , ("uiContextLimit", "contextLimitL")
  , ("uiShowHud", "showHudL")
  , ("uiSelectedSubAgent", "selectedSubAgentL")
  , ("uiPromptHistory", "promptHistoryL")
  ] ''UIState

-- | Provides a standard default UIState to prevent missing-field breakage
defaultUIState :: EngineChannels -> Config -> UIState
defaultUIState chans cfg = UIState
  { uiTurns            = []
  , uiSubAgents        = Map.empty
  , uiCurrentPrompt    = Nothing
  , uiPendingPrompts   = Seq.empty
  , uiMode             = PlanMode
  , uiEditor           = E.editor EditorInput (Just 1) ""
  , uiWorkingState     = ""
  , uiChannels         = chans
  , uiLastEscTime      = Nothing
  , uiContextLimit     = 128000
  , uiPromptHistory    = []
  , uiHistoryIndex     = Nothing
  , uiSavedDraft       = ""
  , uiModelName        = "mock-model"
  , uiThinkingVisible  = True
  , uiSelectedSubAgent = Nothing
  , uiShowHud          = False
  , uiCompletion       = Nothing
  , uiSessionChooser   = Nothing
  , uiIsGenerating     = False
  , uiSpinnerStep      = 0
  , uiExecutionStart   = Nothing
  , uiElapsedSeconds   = 0
  , uiConfig           = cfg
  }
