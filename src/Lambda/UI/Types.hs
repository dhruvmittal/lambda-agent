{-# LANGUAGE OverloadedStrings #-}

module Lambda.UI.Types where

import Brick.Widgets.Edit (Editor)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

import Lambda.Core.EngineInterface (EngineChannels)
import Lambda.Types

data ResourceName
  = ChatView
  | ThinkingFold !Int
  | SubAgentView
  | EditorInput
  | ButtonAlways
  | ButtonOnce
  | ButtonNo
  | ButtonNever
  deriving (Eq, Ord, Show)

data UIState = UIState
  { uiTurns          :: ![Turn]
  , uiSubAgents      :: !(Map Int SubAgentTask)
  , uiCurrentPrompt  :: !(Maybe PermissionPrompt)
  , uiPendingPrompts :: !(Seq PermissionPrompt)
  , uiMode           :: !AgentMode
  , uiEditor         :: !(Editor Text ResourceName)
  , uiWorkingState   :: !Text
  , uiChannels       :: !EngineChannels
  }
