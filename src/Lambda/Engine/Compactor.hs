{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Compactor
  ( sanitizeForApiPayload
  , pruneHistoricalThinking
  , turnsToOpenAIPayload
  , isThinkingBlock
  , estimateTotalTokens
  , defaultAgentSystemPrompt
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lambda.Types

-- | Default persistent systems engineering system prompt anchored across turns
defaultAgentSystemPrompt :: Text
defaultAgentSystemPrompt = T.unlines
  [ "# Identity & Purpose"
  , "You are lambdA, an autonomous, expert systems engineering and coding agent implemented in Haskell."
  , "You operate in an interactive terminal environment with direct access to file inspection, editing, execution, and external tools."
  , ""
  , "# Operational Invariants & Guidelines"
  , "1. **Hypothesis-Driven Problem Solving**:"
  , "   - Before modifying code or executing commands, formulate clear hypotheses about the root cause or objective."
  , "   - Systematically test hypotheses with read-only inspection (e.g. read_file, grep_search, find_by_name, fetch_url) before modifying files."
  , "   - If a file, symbol, or pattern is not found on the first attempt, do NOT abandon the goal. Formulate alternate hypotheses (such as alternative directories, case variants, or wider greps) and verify."
  , ""
  , "2. **Context Mass Economy & Mandatory Subagent Delegation**:"
  , "   - Keep dialogue context lean and high-signal (O(1) context mass principle)."
  , "   - **MANDATORY Codebase Surveying & Large File Delegation**:"
  , "     * In [/plan] mode, or whenever you need to survey a codebase, inspect multiple files, or read files larger than 250 lines, you MUST spawn an ephemeral subagent (`spawn_diagnostic_subagent`) to perform the survey."
  , "     * Subagents in [/plan] mode inherit read-only constraints, operate in an isolated loop, and return compact, structured summaries directly into the conversation."
  , "     * NEVER read entire large files or broad multi-file dumps directly into the primary conversation."
  , "   - **Diagnostics & Traces**:"
  , "     * Delegate large compiler error cascades, test logs, crash dumps, and ASan/TSan traces to subagents as well."
  , ""
  , "3. **Dual-Gate Safety & Mode Discipline**:"
  , "   - In [/plan] mode: Only read-only operations are permitted. Formulate architectures, explore dependencies, and verify assumptions."
  , "   - `spawn_diagnostic_subagent` is fully available in [/plan] mode for read-only surveys, inspections, and architecture planning."
  , "   - In [/exec] mode: Modifying operations (write_file, replace_lines, bash) are enabled, subject to security capability grants."
  , "   - When modifying files, always preserve existing architectural invariants, comments, and style conventions."
  , ""
  , "4. **Communication Style**:"
  , "   - Concise, rigorous, and action-oriented. State findings, root causes, and verification steps clearly without unnecessary filler."
  ]

-- | Strips raw internal reasoning blocks from previous turns before remote API wire egress.
-- Current active turn keeps reasoning so the model maintains its local deduction scratchpad.
sanitizeForApiPayload :: [Turn] -> [Turn]
sanitizeForApiPayload [] = []
sanitizeForApiPayload turns =
  let total = length turns
      sanitizeTurn idx turn
        | idx == total - 1 = turn
        | otherwise        = pruneHistoricalThinking turn
  in zipWith sanitizeTurn [0..] turns

pruneHistoricalThinking :: Turn -> Turn
pruneHistoricalThinking turn =
  turn { turnBlocks = filter (not . isThinkingBlock) (turnBlocks turn) }

isThinkingBlock :: ContentBlock -> Bool
isThinkingBlock (ThinkingBlock {}) = True
isThinkingBlock _                  = False

-- | Converts dialogue turns into standard OpenAI wire-protocol JSON messages.
-- Injects the persistent systems engineering system prompt at the root,
-- and correctly generates native assistant `tool_calls` and `tool` role responses!
turnsToOpenAIPayload :: [Turn] -> [Aeson.Value]
turnsToOpenAIPayload turns =
  let sanitized = sanitizeForApiPayload turns
      isUiBanner (Turn _ SystemRole blks) =
        let txt = T.concat [ t | TextBlock t <- blks ]
        in "lambdA initialized." `T.isPrefixOf` txt
           || "Conversation history cleared." `T.isPrefixOf` txt
           || "[Compaction Checkpoint:" `T.isPrefixOf` txt
           || "⚠️" `T.isPrefixOf` txt
      isUiBanner _ = False
      wireTurns = filter (not . isUiBanner) sanitized
      wireMsgs = concatMap turnToMessages wireTurns
      sysMsg = Aeson.object
        [ "role"    .= ("system" :: Text)
        , "content" .= defaultAgentSystemPrompt
        ]
  in sysMsg : wireMsgs
  where
    turnToMessages :: Turn -> [Aeson.Value]
    turnToMessages (Turn _ role blocks) =
      case role of
        SystemRole ->
          [ Aeson.object
              [ "role"    .= ("system" :: Text)
              , "content" .= renderTextBlocks blocks
              ]
          ]
        UserRole ->
          [ Aeson.object
              [ "role"    .= ("user" :: Text)
              , "content" .= renderTextBlocks blocks
              ]
          ]
        AssistantRole ->
          let toolCalls = mapMaybe extractToolCall blocks
              textContent = renderTextBlocks blocks
          in if null toolCalls
               then [ Aeson.object
                        [ "role"    .= ("assistant" :: Text)
                        , "content" .= textContent
                        ]
                    ]
               else [ Aeson.object
                        [ "role"       .= ("assistant" :: Text)
                        , "content"    .= if T.null textContent then Aeson.Null else Aeson.toJSON textContent
                        , "tool_calls" .= map Aeson.toJSON toolCalls
                        ]
                    ]
        ToolRole ->
          mapMaybe toolResultToMessage blocks

    extractToolCall (ToolCallBlock tc) = Just tc
    extractToolCall _                  = Nothing

    toolResultToMessage (ToolResultBlock ToolResult{..}) =
      let contentText =
            if T.null resultStdout && not (T.null resultStderr)
              then "ERROR: " <> resultStderr
              else if not (T.null resultStderr)
                then resultStdout <> "\nSTDERR:\n" <> resultStderr
                else if T.null resultStdout
                  then "(empty output)"
                  else resultStdout
      in Just $ Aeson.object
           [ "role"         .= ("tool" :: Text)
           , "tool_call_id" .= resultCallIdRef
           , "content"      .= contentText
           ]
    toolResultToMessage _ = Nothing

    renderTextBlocks blks =
      T.concat [ t | TextBlock t <- blks ]

-- | Estimates total tokens currently consumed across dialogue history.
-- Standard rule of thumb: ~4 characters per token across text, reasoning, and tool results.
estimateTotalTokens :: [Turn] -> Int
estimateTotalTokens turns = sum (map estimateTurnTokens turns)
  where
    estimateTurnTokens (Turn _ _ blocks) = sum (map estimateBlockTokens blocks)

    estimateBlockTokens (TextBlock t) = max 1 (T.length t `div` 4)
    estimateBlockTokens (ThinkingBlock _ t _) = max 1 (T.length t `div` 4)
    estimateBlockTokens (ToolCallBlock tc) =
      max 1 ((T.length (toolCallName tc) + 20) `div` 4)
    estimateBlockTokens (ToolResultBlock tr) =
      max 1 ((T.length (resultStdout tr) + T.length (resultStderr tr)) `div` 4)
