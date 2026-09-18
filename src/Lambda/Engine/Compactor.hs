{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Compactor
  ( sanitizeForApiPayload
  , pruneHistoricalThinking
  , turnsToOpenAIPayload
  , isThinkingBlock
  , estimateTotalTokens
  , defaultAgentSystemPrompt
  , compactHistory
  , synthesizeTurnSummary
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
  , "You are lambdA, the Lead Systems Architect and orchestrator implemented in Haskell."
  , "You operate in an interactive terminal environment directing high-level orchestration, hypothesis formulation, task fan-out, and synthesis."
  , ""
  , "# Operational Invariants & Guidelines"
  , "1. **Lead Architect & Specialist Subagent Delegation Doctrine**:"
  , "   - Your role is high-level orchestration, hypothesis formulation, task fan-out, and synthesis."
  , "   - **MANDATORY SPECIALIST DELEGATION**:"
  , "     * Never run profilers (valgrind, perf), sanitizer runs (ASan, TSan), large codebase surveys, test cascades, or large refactorings directly in the main conversation."
  , "     * Always spawn the appropriate specialist (`surveyor`, `debugger`, `profiler`, `implementer`, `reviewer`) using `spawn_specialist_subagent`."
  , "     * Maintain O(1) context mass: specialist subagents run concurrently in isolated loops and return compact, structured reports."
  , "   - **Parallel Specialist Fan-Out**:"
  , "     * When surveying multiple disparate modules, inspecting multiple files, testing competing hypotheses, or running parallel benchmarks, emit multiple `spawn_specialist_subagent` tool calls simultaneously in a single turn. The Haskell runtime executes them concurrently across lightweight green threads."
  , ""
  , "2. **Hypothesis-Driven Problem Solving**:"
  , "   - Formulate clear hypotheses about root cause, performance bottlenecks, or architecture before acting."
  , "   - Verify hypotheses systematically through specialist subagents."
  , ""
  , "3. **Dual-Gate Safety & Mode Discipline**:"
  , "   - In [/plan] mode: Only read-only operations are permitted. Formulate architectures, explore dependencies, and verify assumptions."
  , "   - `spawn_specialist_subagent` is fully available in [/plan] mode for read-only surveys, inspections, profiling analysis, and architecture planning."
  , "   - In [/exec] mode: Modifying operations (write_file, replace_lines, bash) are enabled, subject to security capability grants."
  , "   - When modifying files, always preserve existing architectural invariants, comments, and style conventions."
  , ""
  , "4. **Communication Style**:"
  , "   - Concise, rigorous, and action-oriented. Synthesize specialist findings and state next architectural steps clearly."
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
-- Injects the persistent systems engineering system prompt at the root if no explicit
-- non-banner system message is already present (which subagents have for their persona).
-- Correctly generates native assistant `tool_calls` and `tool` role responses!
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
      hasExplicitSystem = any (\t -> turnRole t == SystemRole) wireTurns
      sysMsg =
        [ Aeson.object
            [ "role"    .= ("system" :: Text)
            , "content" .= defaultAgentSystemPrompt
            ]
        | not hasExplicitSystem
        ]
  in sysMsg ++ wireMsgs
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
      let cleanStderr =
            if T.length resultStderr > 2000
              then T.takeEnd 2000 resultStderr
              else resultStderr
          contentText =
            if T.null resultStdout && not (T.null cleanStderr)
              then "ERROR: " <> cleanStderr
              else if not (T.null cleanStderr)
                then resultStdout <> "\nSTDERR:\n" <> cleanStderr
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

-- | Compacts conversation history when it exceeds token limits or when explicitly triggered.
-- Retains the most recent recentCount turns intact, and synthesizes older turns into a
-- concise context summary turn containing goals, tool calls, and references.
compactHistory :: Int -> Int -> [Turn] -> [Turn]
compactHistory maxTokens recentCount turns
  | estimateTotalTokens turns <= maxTokens = turns
  | length turns <= recentCount + 1 = turns
  | otherwise =
      let rawSplit = splitAt (length turns - recentCount) turns
          (older, recent) = adjustSplit rawSplit
          summaryText = synthesizeTurnSummary older
          minId = case older of
            (t:_) -> turnId t
            []    -> 1
          summaryTurn = Turn minId SystemRole [TextBlock summaryText]
      in if null older
           then recent
           else summaryTurn : recent
  where
    -- Ensures recent turns do not begin with an orphaned ToolRole turn whose
    -- AssistantRole tool call would otherwise be compacted away into the summary.
    adjustSplit (older, recent@(Turn _ ToolRole _:_))
      | not (null older) = adjustSplit (init older, last older : recent)
    adjustSplit pair = pair

-- | Synthesizes a structured summary from a sequence of turns
synthesizeTurnSummary :: [Turn] -> Text
synthesizeTurnSummary turns =
  let totalCompacted = length turns
      userPrompts = [ p | Turn _ UserRole blks <- turns, TextBlock p <- blks ]
      toolCalls   = [ toolCallName tc | Turn _ AssistantRole blks <- turns, ToolCallBlock tc <- blks ]
      artifacts   = [ T.pack path | Turn _ ToolRole blks <- turns, ToolResultBlock tr <- blks, Just path <- [resultArtifactPath tr] ]
      promptSummary = if null userPrompts
                        then "None recorded"
                        else T.intercalate "; " (take 3 userPrompts)
      toolSummary = if null toolCalls
                      then "No tools executed"
                      else T.intercalate ", " (take 5 toolCalls) <> (if length toolCalls > 5 then "..." else "")
      artifactSummary = if null artifacts
                          then "None"
                          else T.intercalate ", " (take 5 artifacts)
  in T.unlines
       [ "[Context Summary: " <> T.pack (show totalCompacted) <> " historical turns compacted into working state]"
       , "- Key Objectives: " <> promptSummary
       , "- Tools Executed: " <> toolSummary
       , "- Active Artifacts: " <> artifactSummary
       ]

