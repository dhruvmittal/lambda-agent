{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Core.ToolProvider
  ( ToolDefinition(..)
  , ToolRegistry(..)
  , emptyRegistry
  , registerTool
  , registerTools
  , lookupTool
  , toolsToOpenAISchema
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Lambda.Types

data ToolDefinition = ToolDefinition
  { toolName        :: !Text
  , toolDescription :: !Text
  , toolParameters  :: !Aeson.Value
  , toolCapability  :: !ToolCapability
  , toolExecute     :: !(CallerContext -> Aeson.Value -> IO ToolResult)
  }

data ToolRegistry = ToolRegistry
  { registeredTools :: !(Map Text ToolDefinition)
  }

emptyRegistry :: ToolRegistry
emptyRegistry = ToolRegistry Map.empty

registerTool :: ToolDefinition -> ToolRegistry -> ToolRegistry
registerTool tool (ToolRegistry m) = ToolRegistry (Map.insert (toolName tool) tool m)

registerTools :: [ToolDefinition] -> ToolRegistry -> ToolRegistry
registerTools tools reg = foldr registerTool reg tools

lookupTool :: Text -> ToolRegistry -> Maybe ToolDefinition
lookupTool name (ToolRegistry m) = Map.lookup name m

-- | Serializes registered tools into OpenAI function calling format.
-- Strictly filters out Destructive tools when in PlanMode!
toolsToOpenAISchema :: AgentMode -> ToolRegistry -> [Aeson.Value]
toolsToOpenAISchema mode (ToolRegistry m) =
  let allTools = Map.elems m
      availableTools = case mode of
        PlanMode -> filter (\t -> toolCapability t == ReadOnly) allTools
        ExecMode -> allTools
  in map toolToJson availableTools
  where
    toolToJson ToolDefinition{..} = Aeson.object
      [ "type"     .= ("function" :: Text)
      , "function" .= Aeson.object
          [ "name"        .= toolName
          , "description" .= toolDescription
          , "parameters"  .= toolParameters
          ]
      ]
