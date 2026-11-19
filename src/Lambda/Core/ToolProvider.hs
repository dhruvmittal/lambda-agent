{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Core.ToolProvider
  ( ToolScope(..)
  , ToolDefinition(..)
  , ToolRegistry(..)
  , emptyRegistry
  , registerTool
  , registerTools
  , lookupTool
  , toolsToOpenAISchema
  , formatMcpCatalog
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Lambda.Types

-- | Execution scope of a tool: RootOnly (orchestrator only, e.g. spawning/resuming subagents)
-- or Universal (callable by both orchestrator and subagents).
data ToolScope
  = RootOnly
  | Universal
  deriving (Eq, Show)

data ToolDefinition = ToolDefinition
  { toolName        :: !Text
  , toolDescription :: !Text
  , toolParameters  :: !Aeson.Value
  , toolCapability  :: !ToolCapability
  , toolIsMcp       :: !Bool
  , toolExecute     :: !(CallerContext -> Maybe Text -> Aeson.Value -> IO ToolResult)
  , toolScope       :: !ToolScope
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

-- | Formats a compact 1-line per tool catalog of available MCP tools for lazy gateway calls
formatMcpCatalog :: AgentMode -> ToolRegistry -> Text
formatMcpCatalog mode (ToolRegistry m) =
  let mcpTools = filter toolIsMcp (Map.elems m)
      availMcp = case mode of
        PlanMode -> filter (\t -> toolCapability t == ReadOnly) mcpTools
        ExecMode -> mcpTools
      catalogLines = [ "- " <> toolName t <> ": " <> toolDescription t | t <- availMcp ]
  in if null catalogLines
       then "Invoke a lazily-loaded MCP tool by name with arguments."
       else "Invoke a lazily-loaded MCP tool by name. Available tools:\n" <> T.unlines catalogLines

-- | Serializes registered tools into OpenAI function calling format.
-- Strictly filters out Destructive tools when in PlanMode, except `bash` which is exposed
-- with a read-only local discovery description and enforced per-segment by Security.hs!
-- Raw MCP tools are hidden behind the call_mcp_tool lazy gateway.
toolsToOpenAISchema :: AgentMode -> ToolRegistry -> [Aeson.Value]
toolsToOpenAISchema mode reg@(ToolRegistry m) =
  let allTools = Map.elems m
      hasMcpTools = any toolIsMcp allTools
      isEligible t
        | toolIsMcp t = False
        | toolName t == "call_mcp_tool" = hasMcpTools
        | otherwise = True
      eagerTools = filter isEligible allTools
      availableTools = case mode of
        PlanMode -> filter (\t -> toolCapability t == ReadOnly || toolName t == "bash") eagerTools
        ExecMode -> eagerTools
  in map toolToJson availableTools
  where
    planBashDesc =
      "Execute a read-only local state discovery command in the workspace directory (Plan mode). "
      <> "Allowed command families: git (status, diff, log, show, blame, rev-parse, ls-files, ls-tree, describe, shortlog, merge-base, symbolic-ref, cat-file, branch --list/-l/--show-current, tag -l, remote -v, stash list), "
      <> "ls, pwd, stat, file, tree, du, df, wc, head, tail, cat, diff, cmp, realpath, readlink, rg, grep, sort, uniq, cut, tr, which, type, uname, ps, id, whoami, hostname, date, env. "
      <> "Mutating commands, output redirects, command substitutions, and network tools are blocked."

    toolToJson ToolDefinition{..} =
      let desc
            | toolName == "call_mcp_tool" = formatMcpCatalog mode reg
            | toolName == "bash" && mode == PlanMode = planBashDesc
            | otherwise = toolDescription
      in Aeson.object
        [ "type"     .= ("function" :: Text)
        , "function" .= Aeson.object
            [ "name"        .= toolName
            , "description" .= desc
            , "parameters"  .= toolParameters
            ]
        ]
