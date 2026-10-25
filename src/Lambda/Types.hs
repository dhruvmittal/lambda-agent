{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Types where

import Control.Concurrent.STM (TMVar)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?), (.!=))
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Operational modes for safety enforcement
data AgentMode = PlanMode | ExecMode
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AgentMode
instance Aeson.FromJSON AgentMode

-- | Capability classification for tools
data ToolCapability = ReadOnly | Destructive
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ToolCapability
instance Aeson.FromJSON ToolCapability

-- | User-facing permission level for operations
data PermissionLevel
  = PermAlways
  | PermSession
  | PermOnce
  | PermDeny
  | PermNo
  | PermNever
  deriving stock (Eq, Show, Ord, Generic)

instance Aeson.ToJSON PermissionLevel
instance Aeson.FromJSON PermissionLevel

-- | Execution caller identity (Primary Orchestrator vs. Ephemeral Worker)
data CallerContext
  = MainAgent
  | SubAgentId !Int !Text
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON CallerContext
instance Aeson.FromJSON CallerContext

-- | Non-blocking authorization request
data PermissionPrompt = PermissionPrompt
  { promptId           :: !Int
  , promptCaller       :: !CallerContext
  , promptTool         :: !Text
  , promptArgs         :: !Aeson.Value
  , promptProposedGlob :: !Text
  , promptReply        :: !(TMVar PermissionLevel)
  }

-- | Folding state for thinking / reasoning blocks
data BlockVisibility = Visible | Collapsed
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON BlockVisibility
instance Aeson.FromJSON BlockVisibility

-- | Structured tool call representation
data ToolCall = ToolCall
  { toolCallId   :: !Text
  , toolCallName :: !Text
  , toolCallArgs :: !Aeson.Value
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ToolCall where
  toJSON ToolCall{..} = Aeson.object
    [ "id"       .= toolCallId
    , "type"     .= ("function" :: Text)
    , "function" .= Aeson.object
        [ "name"      .= toolCallName
        , "arguments" .= encodeArgs toolCallArgs
        ]
    ]
    where
      encodeArgs (Aeson.String s) = s
      encodeArgs val              = TE.decodeUtf8 (BL.toStrict (Aeson.encode val))

instance Aeson.FromJSON ToolCall where
  parseJSON = Aeson.withObject "ToolCall" $ \obj -> do
    toolCallId <- obj .: "id"
    fn <- obj .: "function"
    toolCallName <- fn .: "name"
    toolCallArgs <- fn .: "arguments"
    pure ToolCall{..}

-- | Structured tool execution result
data ToolResult = ToolResult
  { resultCallIdRef    :: !Text
  , resultStdout       :: !Text
  , resultStderr       :: !Text
  , resultArtifactPath :: !(Maybe FilePath)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ToolResult
instance Aeson.FromJSON ToolResult

-- | Unified turn content blocks
data ContentBlock
  = TextBlock !Text
  | ThinkingBlock
      { thinkingId     :: !Int
      , thinkingBody   :: !Text
      , thinkingVisual :: !BlockVisibility
      }
  | ToolCallBlock !ToolCall
  | ToolResultBlock !ToolResult
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ContentBlock
instance Aeson.FromJSON ContentBlock

-- | Dialogue participants
data Role = SystemRole | UserRole | AssistantRole | ToolRole
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Role where
  toJSON = \case
    SystemRole    -> "system"
    UserRole      -> "user"
    AssistantRole -> "assistant"
    ToolRole      -> "tool"

instance Aeson.FromJSON Role where
  parseJSON = Aeson.withText "Role" $ \case
    "system"    -> pure SystemRole
    "user"      -> pure UserRole
    "assistant" -> pure AssistantRole
    "tool"      -> pure ToolRole
    other       -> fail $ "Unknown role: " <> show other

-- | Conversation turn
data Turn = Turn
  { turnId     :: !Int
  , turnRole   :: !Role
  , turnBlocks :: ![ContentBlock]
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Turn
instance Aeson.FromJSON Turn

-- | LLM streaming delta chunks
data StreamChunk
  = ChunkText !Text
  | ChunkThinking !Text
  | ChunkToolCallStart !Text !Text -- CallID, ToolName
  | ChunkToolCallArgs !Text !Text  -- CallID, ArgChunk
  | ChunkDone
  deriving stock (Eq, Show, Generic)

-- | JSON-RPC stdio framing protocol
data Framing = HeaderFramed | LineFramed
  deriving stock (Eq, Show, Generic)

-- | High-level working state / state vector ADT
data WorkingState = WorkingState
  { wsGoal             :: !Text
  , wsInvariants       :: ![Text]
  , wsActiveHypothesis :: !(Maybe Text)
  , wsBlockedOn        :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON WorkingState where
  toJSON WorkingState{..} = Aeson.object
    [ "goal" .= wsGoal
    , "invariants" .= wsInvariants
    , "active_hypothesis" .= wsActiveHypothesis
    , "blocked_on" .= wsBlockedOn
    ]

instance Aeson.FromJSON WorkingState where
  parseJSON (Aeson.String s) = pure (parseWorkingState s)
  parseJSON (Aeson.Object o) =
    WorkingState
      <$> o .:? "goal" .!= "Awaiting task"
      <*> o .:? "invariants" .!= []
      <*> o .:? "active_hypothesis"
      <*> o .:? "blocked_on"
  parseJSON _ = pure (WorkingState "Awaiting task" [] Nothing Nothing)

formatWorkingState :: WorkingState -> Text
formatWorkingState WorkingState{..} =
  T.unlines
    [ "GOAL: " <> wsGoal
    , "INVARIANTS: [" <> T.intercalate ", " wsInvariants <> "]"
    , "ACTIVE_HYPOTHESIS: " <> maybe "None" id wsActiveHypothesis
    , "BLOCKED_ON: " <> maybe "Ready" id wsBlockedOn
    ]

parseWorkingState :: Text -> WorkingState
parseWorkingState txt =
  let ls = T.lines txt
      findPrefix pfx = case filter (pfx `T.isPrefixOf`) ls of
        (l:_) -> Just (T.strip (T.drop (T.length pfx) l))
        []    -> Nothing
      goal = maybe "Awaiting task" id (findPrefix "GOAL:")
      invarsRaw = maybe "" id (findPrefix "INVARIANTS:")
      invars = parseInvariants invarsRaw
      hypo = case findPrefix "ACTIVE_HYPOTHESIS:" of
        Just h | h /= "None" && not (T.null h) -> Just h
        _ -> Nothing
      blocked = case findPrefix "BLOCKED_ON:" of
        Just b | b /= "Ready" && b /= "None" && not (T.null b) -> Just b
        _ -> Nothing
  in WorkingState goal invars hypo blocked
  where
    parseInvariants s =
      let trimmed = T.dropAround (\c -> c == '[' || c == ']' || c == ' ') s
      in if T.null trimmed
           then []
           else filter (not . T.null) (map T.strip (T.splitOn "," trimmed))

-- | Status of ephemeral subagent workers
data SubAgentStatus
  = SubAgentRunning
  | SubAgentSuccess !Text
  | SubAgentBlocked !Text
  | SubAgentFailed !Text
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON SubAgentStatus
instance Aeson.FromJSON SubAgentStatus

-- | Ephemeral SubAgent task tracker descriptor
data SubAgentTask = SubAgentTask
  { subAgentId           :: !Int
  , subAgentRole         :: !Text
  , subAgentHypothesis   :: !Text
  , subAgentTurnCount    :: !Int
  , subAgentBudget       :: !Int
  , subAgentStatus       :: !SubAgentStatus
  , subAgentArtifact     :: !(Maybe FilePath)
  , subAgentTurns        :: ![Turn]
  , subAgentParentTurnId :: !(Maybe Int)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON SubAgentTask
instance Aeson.FromJSON SubAgentTask

-- | Engine to Frontend notification events
data EngineEvent
  = EvTurnAdded !Turn
  | EvTurnUpdated !Turn
  | EvStreamChunk !StreamChunk
  | EvPermissionRequired !PermissionPrompt
  | EvPermissionResolved !Int !PermissionLevel
  | EvSubAgentUpdate !SubAgentTask
  | EvWorkingStateUpdate !Text
  | EvSessionSwitched !Text !AgentMode ![Turn] !(Map Int SubAgentTask)
  | EvModelSwitched !Text !Int
  | EvError !Text
  | EvDone
  | EvTick

-- | Frontend to Engine dispatch commands
data FrontendCommand
  = CmdUserPrompt !Text
  | CmdSetGoal !Text
  | CmdSystemMessage !Text
  | CmdClearHistory
  | CmdSetMode !AgentMode
  | CmdSetModel !Text
  | CmdCancelSubAgent !Int
  | CmdCompactHistory
  | CmdRewindTurns !Int
  | CmdForkSession !(Maybe Text)
  | CmdNewSession
  | CmdSwitchSession !Text
  | CmdExportTrace
  | CmdInterrupt
  | CmdQuit
  deriving stock (Eq, Show, Generic)
