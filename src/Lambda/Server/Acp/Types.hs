{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Server.Acp.Types
  ( AcpClientInfo(..)
  , AcpAgentInfo(..)
  , AcpSessionCapabilities(..)
  , AcpAgentCapabilities(..)
  , AcpInitializeParams(..)
  , AcpInitializeResult(..)
  , AcpSessionNewParams(..)
  , AcpSessionNewResult(..)
  , AcpSessionLoadParams(..)
  , AcpSessionListParams(..)
  , AcpSessionInfo(..)
  , AcpSessionListResult(..)
  , AcpSessionDeleteParams(..)
  , AcpSessionCloseParams(..)
  , AcpSetConfigOptionParams(..)
  , AcpSessionPromptParams(..)
  , AcpSessionPromptResult(..)
  , AcpSetModeParams(..)
  , AcpSessionCancelParams(..)
  , AcpCommand(..)
  , AcpPlanEntry(..)
  , AcpPermissionOption(..)
  , AcpRequestPermissionParams(..)
  , AcpUpdate(..)
  , AcpSessionUpdateNotification(..)
  , AcpJsonRpcRequest(..)
  , AcpJsonRpcResponse(..)
  , AcpJsonRpcNotification(..)
  , AcpIncomingMessage(..)
  , parseAcpPrompt
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data AcpClientInfo = AcpClientInfo
  { clientName    :: !Text
  , clientVersion :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpClientInfo where
  parseJSON = Aeson.withObject "AcpClientInfo" $ \o ->
    AcpClientInfo <$> o .: "name" <*> o .:? "version"

instance Aeson.ToJSON AcpClientInfo where
  toJSON AcpClientInfo{..} = Aeson.object
    [ "name"    .= clientName
    , "version" .= clientVersion
    ]

data AcpAgentInfo = AcpAgentInfo
  { agentName    :: !Text
  , agentVersion :: !Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpAgentInfo where
  toJSON AcpAgentInfo{..} = Aeson.object
    [ "name"    .= agentName
    , "version" .= agentVersion
    ]

instance Aeson.FromJSON AcpAgentInfo where
  parseJSON = Aeson.withObject "AcpAgentInfo" $ \o ->
    AcpAgentInfo <$> o .: "name" <*> o .: "version"

data AcpSessionCapabilities = AcpSessionCapabilities
  { capList            :: !(Maybe Aeson.Object)
  , capDelete          :: !(Maybe Aeson.Object)
  , capClose           :: !(Maybe Aeson.Object)
  , capSetConfigOption :: !(Maybe Aeson.Object)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpSessionCapabilities where
  toJSON AcpSessionCapabilities{..} = Aeson.object $ catMaybes
    [ ("list" .=) <$> capList
    , ("delete" .=) <$> capDelete
    , ("close" .=) <$> capClose
    , ("setConfigOption" .=) <$> capSetConfigOption
    ]

instance Aeson.FromJSON AcpSessionCapabilities where
  parseJSON = Aeson.withObject "AcpSessionCapabilities" $ \o ->
    AcpSessionCapabilities
      <$> o .:? "list"
      <*> o .:? "delete"
      <*> o .:? "close"
      <*> o .:? "setConfigOption"

data AcpAgentCapabilities = AcpAgentCapabilities
  { capLoadSession         :: !Bool
  , capPromptCapabilities  :: !(Maybe Aeson.Value)
  , capSessionCapabilities :: !(Maybe AcpSessionCapabilities)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpAgentCapabilities where
  toJSON AcpAgentCapabilities{..} = Aeson.object $
    [ "loadSession"        .= capLoadSession
    , "promptCapabilities" .= capPromptCapabilities
    ] ++ maybe [] (\sc -> ["sessionCapabilities" .= sc]) capSessionCapabilities

instance Aeson.FromJSON AcpAgentCapabilities where
  parseJSON = Aeson.withObject "AcpAgentCapabilities" $ \o ->
    AcpAgentCapabilities
      <$> (o .:? "loadSession" >>= pure . maybe False id)
      <*> o .:? "promptCapabilities"
      <*> o .:? "sessionCapabilities"

data AcpInitializeParams = AcpInitializeParams
  { initProtocolVersion   :: !Int
  , initClientInfo        :: !(Maybe AcpClientInfo)
  , initClientCapabilities :: !(Maybe Aeson.Object)
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpInitializeParams where
  parseJSON = Aeson.withObject "AcpInitializeParams" $ \o ->
    AcpInitializeParams
      <$> o .: "protocolVersion"
      <*> o .:? "clientInfo"
      <*> o .:? "clientCapabilities"

data AcpInitializeResult = AcpInitializeResult
  { initRespProtocolVersion :: !Int
  , initRespAgentInfo       :: !AcpAgentInfo
  , initRespCapabilities    :: !AcpAgentCapabilities
  , initRespAuthMethods     :: ![Text]
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpInitializeResult where
  toJSON AcpInitializeResult{..} = Aeson.object
    [ "protocolVersion"   .= initRespProtocolVersion
    , "agentInfo"         .= initRespAgentInfo
    , "agentCapabilities" .= initRespCapabilities
    , "authMethods"       .= initRespAuthMethods
    ]

instance Aeson.FromJSON AcpInitializeResult where
  parseJSON = Aeson.withObject "AcpInitializeResult" $ \o ->
    AcpInitializeResult
      <$> o .: "protocolVersion"
      <*> o .: "agentInfo"
      <*> o .: "agentCapabilities"
      <*> (o .:? "authMethods" >>= pure . maybe [] id)

data AcpSessionNewParams = AcpSessionNewParams
  { newSessionCwd        :: !(Maybe FilePath)
  , newSessionMcpServers :: !(Maybe [Aeson.Value])
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSessionNewParams where
  parseJSON = Aeson.withObject "AcpSessionNewParams" $ \o ->
    AcpSessionNewParams
      <$> o .:? "cwd"
      <*> o .:? "mcpServers"

newtype AcpSessionNewResult = AcpSessionNewResult
  { newSessionId :: Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpSessionNewResult where
  toJSON AcpSessionNewResult{..} = Aeson.object
    [ "sessionId" .= newSessionId
    ]

instance Aeson.FromJSON AcpSessionNewResult where
  parseJSON = Aeson.withObject "AcpSessionNewResult" $ \o ->
    AcpSessionNewResult <$> o .: "sessionId"

data AcpSessionLoadParams = AcpSessionLoadParams
  { loadSessionId  :: !Text
  , loadSessionCwd :: !(Maybe FilePath)
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSessionLoadParams where
  parseJSON = Aeson.withObject "AcpSessionLoadParams" $ \o ->
    AcpSessionLoadParams
      <$> o .: "sessionId"
      <*> o .:? "cwd"

data AcpSessionListParams = AcpSessionListParams
  { listSessionCwd    :: !(Maybe FilePath)
  , listSessionCursor :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSessionListParams where
  parseJSON = Aeson.withObject "AcpSessionListParams" $ \o ->
    AcpSessionListParams
      <$> o .:? "cwd"
      <*> o .:? "cursor"

data AcpSessionInfo = AcpSessionInfo
  { infoSessionId :: !Text
  , infoCwd       :: !(Maybe FilePath)
  , infoTitle     :: !(Maybe Text)
  , infoUpdatedAt :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpSessionInfo where
  toJSON AcpSessionInfo{..} = Aeson.object $
    [ "sessionId" .= infoSessionId
    ] ++ maybe [] (\c -> ["cwd" .= c]) infoCwd
      ++ maybe [] (\t -> ["title" .= t]) infoTitle
      ++ maybe [] (\u -> ["updatedAt" .= u]) infoUpdatedAt

instance Aeson.FromJSON AcpSessionInfo where
  parseJSON = Aeson.withObject "AcpSessionInfo" $ \o ->
    AcpSessionInfo
      <$> o .: "sessionId"
      <*> o .:? "cwd"
      <*> o .:? "title"
      <*> o .:? "updatedAt"

data AcpSessionListResult = AcpSessionListResult
  { listSessionsList :: ![AcpSessionInfo]
  , listNextCursor   :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpSessionListResult where
  toJSON AcpSessionListResult{..} = Aeson.object $
    [ "sessions"   .= listSessionsList
    , "nextCursor" .= listNextCursor
    ]

instance Aeson.FromJSON AcpSessionListResult where
  parseJSON = Aeson.withObject "AcpSessionListResult" $ \o ->
    AcpSessionListResult
      <$> o .: "sessions"
      <*> o .:? "nextCursor"

newtype AcpSessionDeleteParams = AcpSessionDeleteParams
  { deleteSessionId :: Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSessionDeleteParams where
  parseJSON = Aeson.withObject "AcpSessionDeleteParams" $ \o ->
    AcpSessionDeleteParams <$> o .: "sessionId"

newtype AcpSessionCloseParams = AcpSessionCloseParams
  { closeSessionId :: Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSessionCloseParams where
  parseJSON = Aeson.withObject "AcpSessionCloseParams" $ \o ->
    AcpSessionCloseParams <$> o .: "sessionId"

data AcpSetConfigOptionParams = AcpSetConfigOptionParams
  { setConfigSessionId :: !Text
  , setConfigId        :: !Text
  , setConfigValue     :: !Aeson.Value
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSetConfigOptionParams where
  parseJSON = Aeson.withObject "AcpSetConfigOptionParams" $ \o ->
    AcpSetConfigOptionParams
      <$> o .: "sessionId"
      <*> o .: "configId"
      <*> o .: "value"

data AcpSessionPromptParams = AcpSessionPromptParams
  { promptSessionId :: !Text
  , promptText      :: !Text
  } deriving stock (Eq, Show, Generic)

-- | Parses prompt parameter whether sent as raw string or structured object/list
parseAcpPrompt :: Aeson.Value -> Text
parseAcpPrompt (Aeson.String s) = s
parseAcpPrompt (Aeson.Object o) =
  case parseEither (.: "text") o of
    Right t -> t
    Left _  -> case parseEither (.: "content") o of
      Right (Aeson.String c) -> c
      _ -> ""
parseAcpPrompt (Aeson.Array arr) =
  T.intercalate "\n" [ parseAcpPrompt item | item <- foldr (:) [] arr ]
parseAcpPrompt _ = ""

instance Aeson.FromJSON AcpSessionPromptParams where
  parseJSON = Aeson.withObject "AcpSessionPromptParams" $ \o -> do
    sId <- o .: "sessionId"
    rawPromptVal <- o .: "prompt"
    pure $ AcpSessionPromptParams sId (parseAcpPrompt rawPromptVal)

newtype AcpSessionPromptResult = AcpSessionPromptResult
  { promptStopReason :: Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpSessionPromptResult where
  toJSON AcpSessionPromptResult{..} = Aeson.object
    [ "stopReason" .= promptStopReason
    ]

instance Aeson.FromJSON AcpSessionPromptResult where
  parseJSON = Aeson.withObject "AcpSessionPromptResult" $ \o ->
    AcpSessionPromptResult <$> o .: "stopReason"

data AcpSetModeParams = AcpSetModeParams
  { setModeSessionId :: !Text
  , setModeTarget    :: !Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSetModeParams where
  parseJSON = Aeson.withObject "AcpSetModeParams" $ \o ->
    AcpSetModeParams
      <$> o .: "sessionId"
      <*> o .: "mode"

newtype AcpSessionCancelParams = AcpSessionCancelParams
  { cancelSessionId :: Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpSessionCancelParams where
  parseJSON = Aeson.withObject "AcpSessionCancelParams" $ \o ->
    AcpSessionCancelParams <$> o .: "sessionId"

data AcpCommand = AcpCommand
  { cmdName        :: !Text
  , cmdDescription :: !Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpCommand where
  toJSON AcpCommand{..} = Aeson.object
    [ "name"        .= cmdName
    , "description" .= cmdDescription
    ]

data AcpPlanEntry = AcpPlanEntry
  { planContent  :: !Text
  , planStatus   :: !Text
  , planPriority :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpPlanEntry where
  toJSON AcpPlanEntry{..} = Aeson.object $
    [ "content" .= Aeson.object [ "type" .= ("text" :: Text), "text" .= planContent ]
    , "status"  .= planStatus
    ] ++ maybe [] (\p -> ["priority" .= p]) planPriority

instance Aeson.FromJSON AcpPlanEntry where
  parseJSON = Aeson.withObject "AcpPlanEntry" $ \o -> do
    c <- o .: "content"
    txt <- c .: "text"
    st <- o .: "status"
    pr <- o .:? "priority"
    pure $ AcpPlanEntry txt st pr

data AcpPermissionOption = AcpPermissionOption
  { optId    :: !Text
  , optLabel :: !Text
  , optKind  :: !Text
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpPermissionOption where
  toJSON AcpPermissionOption{..} = Aeson.object
    [ "optionId" .= optId
    , "label"    .= optLabel
    , "kind"     .= optKind
    ]

instance Aeson.FromJSON AcpPermissionOption where
  parseJSON = Aeson.withObject "AcpPermissionOption" $ \o ->
    AcpPermissionOption
      <$> o .: "optionId"
      <*> o .: "label"
      <*> o .: "kind"

data AcpRequestPermissionParams = AcpRequestPermissionParams
  { permSessionId :: !Text
  , permToolCall  :: !Aeson.Value
  , permOptions   :: ![AcpPermissionOption]
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpRequestPermissionParams where
  toJSON AcpRequestPermissionParams{..} = Aeson.object
    [ "sessionId" .= permSessionId
    , "toolCall"  .= permToolCall
    , "options"   .= permOptions
    ]

instance Aeson.FromJSON AcpRequestPermissionParams where
  parseJSON = Aeson.withObject "AcpRequestPermissionParams" $ \o ->
    AcpRequestPermissionParams
      <$> o .: "sessionId"
      <*> o .: "toolCall"
      <*> o .: "options"

data AcpUpdate
  = AcpAgentMessageChunk !Text
  | AcpAgentThoughtChunk !Text
  | AcpToolCall !Text !Text !Text !Text -- callId, title, kind, status
  | AcpToolCallUpdate !Text !Text !(Maybe Text) -- callId, status, content
  | AcpCurrentModeUpdate !Text
  | AcpAvailableCommandsUpdate ![AcpCommand]
  | AcpPlanUpdate ![AcpPlanEntry]
  | AcpUsageUpdate !Int !Int !(Maybe Double) -- totalTokens, promptTokens, cost
  | AcpSessionInfoUpdate !Text -- title
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpUpdate where
  toJSON (AcpAgentMessageChunk txt) = Aeson.object
    [ "sessionUpdate" .= ("agent_message_chunk" :: Text)
    , "content"       .= Aeson.object [ "type" .= ("text" :: Text), "text" .= txt ]
    ]
  toJSON (AcpAgentThoughtChunk thk) = Aeson.object
    [ "sessionUpdate" .= ("agent_thought_chunk" :: Text)
    , "content"       .= Aeson.object [ "type" .= ("text" :: Text), "text" .= thk ]
    ]
  toJSON (AcpToolCall callId title kind status) = Aeson.object
    [ "sessionUpdate" .= ("tool_call" :: Text)
    , "toolCallId"    .= callId
    , "title"         .= title
    , "kind"          .= kind
    , "status"        .= status
    ]
  toJSON (AcpToolCallUpdate callId status mContent) = Aeson.object
    [ "sessionUpdate" .= ("tool_call_update" :: Text)
    , "toolCallId"    .= callId
    , "status"        .= status
    , "content"       .= case mContent of
        Just c  -> [ Aeson.object [ "type" .= ("text" :: Text), "text" .= c ] ]
        Nothing -> []
    ]
  toJSON (AcpCurrentModeUpdate m) = Aeson.object
    [ "sessionUpdate" .= ("current_mode_update" :: Text)
    , "mode"          .= m
    ]
  toJSON (AcpAvailableCommandsUpdate cmds) = Aeson.object
    [ "sessionUpdate" .= ("available_commands_update" :: Text)
    , "commands"      .= cmds
    ]
  toJSON (AcpPlanUpdate entries) = Aeson.object
    [ "sessionUpdate" .= ("plan" :: Text)
    , "entries"       .= entries
    ]
  toJSON (AcpUsageUpdate totalToks promptToks mCost) = Aeson.object $
    [ "sessionUpdate" .= ("session_usage_update" :: Text)
    , "totalTokens"   .= totalToks
    , "promptTokens"  .= promptToks
    ] ++ maybe [] (\c -> ["cost" .= c]) mCost
  toJSON (AcpSessionInfoUpdate title) = Aeson.object
    [ "sessionUpdate" .= ("session_info_update" :: Text)
    , "title"         .= title
    ]

data AcpSessionUpdateNotification = AcpSessionUpdateNotification
  { notifySessionId :: !Text
  , notifyUpdate    :: !AcpUpdate
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpSessionUpdateNotification where
  toJSON AcpSessionUpdateNotification{..} = Aeson.object
    [ "sessionId" .= notifySessionId
    , "update"    .= notifyUpdate
    ]

-- | JSON-RPC 2.0 Envelope Models
data AcpJsonRpcRequest = AcpJsonRpcRequest
  { reqId     :: !(Maybe Aeson.Value)
  , reqMethod :: !Text
  , reqParams :: !(Maybe Aeson.Value)
  } deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpJsonRpcRequest where
  parseJSON = Aeson.withObject "AcpJsonRpcRequest" $ \o ->
    AcpJsonRpcRequest
      <$> o .:? "id"
      <*> o .: "method"
      <*> o .:? "params"

data AcpJsonRpcResponse = AcpJsonRpcResponse
  { respId     :: !Aeson.Value
  , respResult :: !(Maybe Aeson.Value)
  , respError  :: !(Maybe Aeson.Value)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpJsonRpcResponse where
  toJSON AcpJsonRpcResponse{..} = Aeson.object $
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= respId
    ] ++ maybe [] (\r -> ["result" .= r]) respResult
      ++ maybe [] (\e -> ["error"  .= e]) respError

instance Aeson.FromJSON AcpJsonRpcResponse where
  parseJSON = Aeson.withObject "AcpJsonRpcResponse" $ \o ->
    AcpJsonRpcResponse
      <$> o .: "id"
      <*> o .:? "result"
      <*> o .:? "error"

data AcpJsonRpcNotification = AcpJsonRpcNotification
  { notifMethod :: !Text
  , notifParams :: !Aeson.Value
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON AcpJsonRpcNotification where
  toJSON AcpJsonRpcNotification{..} = Aeson.object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method"  .= notifMethod
    , "params"  .= notifParams
    ]

instance Aeson.FromJSON AcpJsonRpcNotification where
  parseJSON = Aeson.withObject "AcpJsonRpcNotification" $ \o ->
    AcpJsonRpcNotification
      <$> o .: "method"
      <*> o .: "params"

data AcpIncomingMessage
  = IncomingRequest !AcpJsonRpcRequest
  | IncomingResponse !AcpJsonRpcResponse
  deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON AcpIncomingMessage where
  parseJSON = Aeson.withObject "AcpIncomingMessage" $ \o -> do
    mMethod <- o .:? "method"
    case mMethod of
      Just m -> do
        rId <- o .:? "id"
        params <- o .:? "params"
        pure $ IncomingRequest (AcpJsonRpcRequest rId m params)
      Nothing -> do
        rId <- o .: "id"
        mRes <- o .:? "result"
        mErr <- o .:? "error"
        pure $ IncomingResponse (AcpJsonRpcResponse rId mRes mErr)

