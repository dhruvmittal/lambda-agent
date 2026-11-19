{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Compactor
  ( SanitizedTurns(..)
  , sanitizeForApiPayload
  , pruneHistoricalThinking
  , turnsToOpenAIPayload
  , turnsToOpenAIPayloadWithSystem
  , buildRootSystemPrompt
  , isThinkingBlock
  , estimateTotalTokens
  , compactionThreshold
  , defaultAgentSystemPrompt
  , compactHistory
  , synthesizeTurnSummary
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lambda.Types

-- | Default persistent systems engineering system prompt anchored across turns
defaultAgentSystemPrompt :: Text
defaultAgentSystemPrompt = T.unlines
  [ "# Identity & Purpose"
  , "You are lambdA, the Lead Systems Architect and orchestrator implemented in Haskell."
  , "You operate in an interactive terminal environment directing high-level orchestration, hypothesis formulation, parallel specialist fan-out, and synthesis."
  , ""
  , "# Operational Invariants & Guidelines"
  , "1. **Parallel Specialist Delegation Doctrine**:"
  , "   - Maintain O(1) context mass: delegate multi-file surveys, debugging, profiling, test cascades, and deep edits to specialist subagents (`surveyor`, `debugger`, `profiler`, `implementer`, `reviewer`) via `spawn_specialist_subagent`."
  , "   - **Default to Parallel Fan-Out**: Whenever a task spans 2+ modules, has 2+ hypotheses, or splits into independent edits, emit multiple `spawn_specialist_subagent` calls in a single turn. Handle trivial single-target lookups directly when spawning would add pure latency."
  , "   - **Disjoint File Ownership**: Never assign two concurrent `implementer` subagents to edit the same file."
  , "   - **Self-Contained Task Briefs**: Include exact file paths, symbols, constraints, and acceptance criteria in `task` so workers succeed on turn 1."
  , "   - **Resume-to-Pivot**: Every `<specialist_report id=\"N\">` includes a subagent ID. Prefer `resume_subagent` (with `specialize_role`) to pivot a warm `surveyor` or `debugger` into an `implementer` without re-reading files."
  , "   - **Parallel Verify & Review**: After edits, fan out `reviewer` and verification checks concurrently."
  , ""
  , "2. **Hypothesis-Driven Problem Solving**:"
  , "   - Formulate clear hypotheses about root cause, performance bottlenecks, or architecture before acting."
  , "   - Verify hypotheses systematically through specialist subagents."
  , ""
  , "3. **Dual-Gate Safety & Mode Discipline**:"
  , "   - In [/plan] mode: Only read-only operations are permitted—including `read_file`, `list_directory`, read-only `bash` local discovery (`git`, `rg`, `ls`, `stat`, `ps`, `diff`, etc.), MCP read tools, and `spawn_specialist_subagent` for parallel surveys and architecture planning."
  , "   - In [/exec] mode: Modifying operations (`write_file`, `edit_file`, `bash`) are enabled, subject to security capability grants."
  , "   - When modifying files, preserve existing architectural invariants, comments, and style conventions."
  , ""
  , "4. **Communication Style**:"
  , "   - Concise, rigorous, and action-oriented. Synthesize specialist findings and state next architectural steps clearly."
  , ""
  , "5. **Persistent Memory & MCP Tools**:"
  , "   - External MCP tools (like StormDrain persistent memory) are accessed via `call_mcp_tool`."
  , "   - Available tools and parameters are listed in the `call_mcp_tool` function specification."
  , "   - Always invoke `call_mcp_tool` with `{\"name\": \"<tool_name>\", \"arguments\": { ... }}`."
  , ""
  , "6. **File Reading & Tool Discipline**:"
  , "   - Always use `read_file` (or specialist subagents) for local workspace files and `file://` URIs."
  , "   - Never use `fetch_url` for local files or `file://` links; `fetch_url` is exclusively for remote HTTP/HTTPS endpoints."
  ]

-- | Builds the static, prompt-cache-stable root system prompt for the orchestrator,
-- appending workspace root and capped hierarchical project instructions when present.
buildRootSystemPrompt :: FilePath -> Text -> Text
buildRootSystemPrompt wsRoot projCtx =
  let wsSection = "\n## Workspace\n- Root: " <> T.pack wsRoot
      ctxSection = if T.null (T.strip projCtx) then "" else "\n\n" <> T.strip projCtx
  in T.stripEnd defaultAgentSystemPrompt <> "\n" <> wsSection <> ctxSection <> "\n"

-- | Opaque wrapper guaranteeing that historical reasoning blocks have been pruned before egress.
newtype SanitizedTurns = SanitizedTurns { unSanitizedTurns :: [Turn] }
  deriving (Eq, Show)

-- | Strips raw internal reasoning blocks from previous turns before remote API wire egress.
-- Current active turn keeps reasoning so the model maintains its local deduction scratchpad.
sanitizeForApiPayload :: [Turn] -> SanitizedTurns
sanitizeForApiPayload [] = SanitizedTurns []
sanitizeForApiPayload turns =
  let total = length turns
      sanitizeTurn idx turn
        | idx == total - 1 = turn
        | otherwise        = pruneHistoricalThinking turn
  in SanitizedTurns (zipWith sanitizeTurn [0..] turns)

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
turnsToOpenAIPayload = turnsToOpenAIPayloadWithSystem Nothing

-- | Converts dialogue turns into OpenAI wire-protocol JSON messages with an optional
-- enriched root system prompt. Ensures mid-conversation SystemRole turns (compaction
-- summaries, /goal, /model notifications) never suppress the root system prompt or emit
-- mid-stream role:"system" messages, and coalesces adjacent user messages.
turnsToOpenAIPayloadWithSystem :: Maybe Text -> [Turn] -> [Aeson.Value]
turnsToOpenAIPayloadWithSystem mRootSys turns =
  let SanitizedTurns sanitized = sanitizeForApiPayload turns
      isUiBanner (Turn _ SystemRole blks) =
        let txt = T.concat [ t | TextBlock t <- blks ]
        in "lambdA initialized." `T.isPrefixOf` txt
           || "Conversation history cleared." `T.isPrefixOf` txt
           || "[Compaction Checkpoint:" `T.isPrefixOf` txt
           || "⚠️" `T.isPrefixOf` txt
      isUiBanner _ = False
      isRuntimeSystemText txt =
        "[Context Summary:" `T.isPrefixOf` txt
        || "Goal set to:" `T.isPrefixOf` txt
        || "Switched active model" `T.isPrefixOf` txt
        || "Resuming SubAgent" `T.isPrefixOf` txt
        || "Debug execution trace" `T.isPrefixOf` txt
        || "<specialist_report" `T.isPrefixOf` txt
      wireTurns = filter (not . isUiBanner) sanitized
      hasLeadingPersonaSystem = case wireTurns of
        (Turn _ SystemRole blks : _) -> not (isRuntimeSystemText (renderTextBlocks blks))
        _                            -> False
      rootContent = fromMaybe defaultAgentSystemPrompt mRootSys
      sysMsg =
        [ Aeson.object
            [ "role"    .= ("system" :: Text)
            , "content" .= rootContent
            ]
        | not hasLeadingPersonaSystem
        ]
      wireMsgs = concat (zipWith turnToMessages [0 :: Int ..] wireTurns)
  in coalesceUserMessages (sysMsg ++ wireMsgs)
  where
    turnToMessages :: Int -> Turn -> [Aeson.Value]
    turnToMessages idx (Turn _ role blocks) =
      case role of
        SystemRole ->
          let txt = renderTextBlocks blocks
          in if idx == 0 && not (isRuntimeSystemText txt)
               then [ Aeson.object
                        [ "role"    .= ("system" :: Text)
                        , "content" .= txt
                        ]
                    ]
               else
                 let wrapped = if "[Context Summary:" `T.isPrefixOf` txt
                       then "<compaction_summary>\n" <> txt <> "\n</compaction_summary>"
                       else txt
                 in [ Aeson.object
                        [ "role"    .= ("user" :: Text)
                        , "content" .= wrapped
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

    isRuntimeSystemText txt =
      "[Context Summary:" `T.isPrefixOf` txt
      || "Goal set to:" `T.isPrefixOf` txt
      || "Switched active model" `T.isPrefixOf` txt
      || "Resuming SubAgent" `T.isPrefixOf` txt
      || "Debug execution trace" `T.isPrefixOf` txt
      || "<specialist_report" `T.isPrefixOf` txt

    coalesceUserMessages :: [Aeson.Value] -> [Aeson.Value]
    coalesceUserMessages [] = []
    coalesceUserMessages (m1 : m2 : rest)
      | Just c1 <- extractUserContent m1
      , Just c2 <- extractUserContent m2 =
          let merged = Aeson.object
                [ "role"    .= ("user" :: Text)
                , "content" .= (c1 <> "\n\n" <> c2)
                ]
          in coalesceUserMessages (merged : rest)
    coalesceUserMessages (m : rest) = m : coalesceUserMessages rest

    extractUserContent :: Aeson.Value -> Maybe Text
    extractUserContent = parseMaybe $ Aeson.withObject "msg" $ \o -> do
      r <- o .: "role"
      if (r :: Text) == "user"
        then o .: "content"
        else fail "not user"

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
      let argsChars = fromIntegral (BL.length (Aeson.encode (toolCallArgs tc)))
      in max 1 ((T.length (toolCallName tc) + argsChars + 20) `div` 4)
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

-- | Computes context-rot-aware auto-compaction trigger: min(60% of window, 200k tokens),
-- subtracting a 4,000-token system/tool reserve on real-sized windows while remaining safe
-- for small mock test limits.
compactionThreshold :: Int -> Int
compactionThreshold limit =
  let scaled  = min ((limit * 60) `div` 100) 200000
      reserve = if scaled > 8000 then 4000 else 0
  in max 64 (scaled - reserve)

-- | Synthesizes a structured high-signal summary from a sequence of turns
synthesizeTurnSummary :: [Turn] -> Text
synthesizeTurnSummary turns =
  let totalCompacted = length turns
      userPrompts = [ p | Turn _ UserRole blks <- turns, TextBlock p <- blks, not (T.null (T.strip p)) ]
      goalUpdates = [ T.strip (T.drop 12 s) | Turn _ SystemRole blks <- turns, TextBlock s <- blks, "Goal set to:" `T.isPrefixOf` s ]
      toolCalls   = [ tc | Turn _ AssistantRole blks <- turns, ToolCallBlock tc <- blks ]
      toolNames   = nubOrd (map toolCallName toolCalls)
      extractPath tc = case toolCallName tc of
        n | n `elem` ["read_file", "edit_file", "write_file", "list_directory"] ->
              parseMaybe (Aeson.withObject "args" (Aeson..: "path")) (toolCallArgs tc)
        _ -> Nothing
      filesTouched = nubOrd (mapMaybe extractPath toolCalls)
      specialistReports =
        [ T.take 200 (T.replace "\n" " " (T.strip out))
        | Turn _ ToolRole blks <- turns
        , ToolResultBlock tr <- blks
        , let out = resultStdout tr
        , "<specialist_report" `T.isInfixOf` out
        ]
      assistantFindings =
        [ T.take 240 (T.replace "\n" " " (T.strip t))
        | Turn _ AssistantRole blks <- reverse turns
        , TextBlock t <- blks
        , not (T.null (T.strip t))
        , not ("Thinking..." == T.strip t)
        ]
      artifacts   = nubOrd [ T.pack path | Turn _ ToolRole blks <- turns, ToolResultBlock tr <- blks, Just path <- [resultArtifactPath tr] ]
      priorSummaries = [ s | Turn _ SystemRole blks <- turns, TextBlock s <- blks, "[Context Summary:" `T.isInfixOf` s ]
      objectives = goalUpdates ++ take 3 userPrompts
      promptSummary = if null objectives
                        then "None recorded"
                        else T.intercalate "; " (take 4 objectives)
      toolSummary = if null toolNames
                      then "No tools executed"
                      else T.intercalate ", " (take 8 toolNames) <> (if length toolNames > 8 then "..." else "")
      artifactSummary = if null artifacts
                          then "None"
                          else T.intercalate ", " (take 5 artifacts)
      filesSection =
        [ "- Files Touched: " <> T.intercalate ", " (take 12 filesTouched)
        | not (null filesTouched)
        ]
      reportsSection =
        [ "- Specialist Reports: " <> T.intercalate " | " (take 4 specialistReports)
        | not (null specialistReports)
        ]
      findingsSection =
        [ "- Recent Findings: " <> T.intercalate " | " (reverse (take 2 assistantFindings))
        | not (null assistantFindings)
        ]
      priorSection = case priorSummaries of
        [] -> []
        (p:_) -> ["- Prior Context: " <> T.strip (T.take 300 (T.replace "\n" " " p))]
  in T.unlines $
       [ "[Context Summary: " <> T.pack (show totalCompacted) <> " historical turns compacted into working state]"
       , "- Key Objectives: " <> promptSummary
       , "- Tools Executed: " <> toolSummary
       , "- Active Artifacts: " <> artifactSummary
       ] ++ filesSection ++ reportsSection ++ findingsSection ++ priorSection

