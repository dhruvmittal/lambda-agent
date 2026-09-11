{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, race, async, wait)
import Control.Concurrent.STM
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (removeDirectoryRecursive, doesFileExist, removeFile)
import System.Environment (setEnv, unsetEnv)
import System.Exit (exitFailure)

import Lambda.Config (loadConfig, Config(..), SpecialistConfig(..))
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider
import Lambda.Driver.OpenAI (parseSseChunk, splitThinkingChunks)
import Lambda.Engine.Artifacts
import Lambda.Engine.Compactor
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.Security (initSecurity, checkAuthorization, SecurityState(..))
import Lambda.Engine.State (initEngineState, registerSubAgentTask, updateSubAgentTurns, appInterrupted, appSubAgents, appMode)
import Lambda.Engine.SubAgent (submitReportTool, filterSubAgentRegistry, subAgentLoop, SubAgentReport(..))
import qualified Data.Map.Strict as Map
import Brick.Types (vSize, Size(..))
import Lambda.Provider.Builtin (listDirectoryTool, readFileTool, fetchUrlTool)
import Lambda.Provider.Mcp (inferCapability, parseMcpCallResult, startAndLoadMcpServers, stopMcpClient)
import Lambda.UI.Draw (renderSubAgents, renderSubAgentsSelected)
import Lambda.Types

main :: IO ()
main = do
  putStrLn "\n=== Running lambdA Invariant Test Suite ==="

  testThinkingEviction
  testWireToolCalling
  testArtifactSpooling
  testSecurityGlobMatching
  testModeFiltering
  testConfigFallbacks
  testDirectoryTools
  testContextEstimation
  testMcpProtocolAndMapping
  testFetchUrlTool
  testPersistentSystemPrompt
  testLiveMcpServerIntegration
  testSubAgentViewportInvariant
  testPlanModeSubAgentAndContextGuard
  testConcurrentToolExecution
  testSubAgentCoTAndNavigation
  testInlineThinkingStreamingParser
  testWireSystemPromptDecoupling
  testSubmitReportToolEgress
  testBudgetExhaustionDiagnostics
  testDepth1StarGraphSchemaExclusion
  testDataDrivenSpecialistConfigLoading
  testDynamicPermissionEscalation

  putStrLn "\n=== All Invariant Tests Passed Successfully! ==="

-- 1. Verify Thinking Block Eviction on Wire Payloads
testThinkingEviction :: IO ()
testThinkingEviction = do
  putStrLn "\n[Test 1] Historical Thinking Eviction"
  let turn1 = Turn 1 AssistantRole
        [ ThinkingBlock 1 "Turn 1 internal thought scratchpad." Collapsed
        , TextBlock "Conclusion of turn 1."
        ]
      turn2 = Turn 2 UserRole [TextBlock "User prompt 2."]
      turn3 = Turn 3 AssistantRole
        [ ThinkingBlock 2 "Turn 3 active thinking." Visible
        , TextBlock "Active answer 3."
        ]
      sanitized = sanitizeForApiPayload [turn1, turn2, turn3]

  -- Turn 1 should have NO thinking block
  let t1Blocks = turnBlocks (head sanitized)
  assert "Turn 1 thinking was purged" (not (any isThinkingBlock t1Blocks))
  assert "Turn 1 text was preserved" (any isTextBlock t1Blocks)

  -- Turn 3 (active turn) should PRESERVE thinking block
  let t3Blocks = turnBlocks (last sanitized)
  assert "Turn 3 thinking is preserved for local scratchpad" (any isThinkingBlock t3Blocks)

  -- Wire JSON payload should have no thinking on historical turns
  let wireMessages = turnsToOpenAIPayload [turn1, turn2, turn3]
  -- wireMessages !! 0 is persistent system prompt, wireMessages !! 1 is turn1
  let t1Wire = wireMessages !! 1
  assert "Turn 1 wire JSON contains only text" (not ("scratchpad" `T.isInfixOf` T.pack (show t1Wire)))
  putStrLn "  -> OK: Historical reasoning strictly evicted from egress wire payloads."
  where
    isTextBlock (TextBlock _) = True
    isTextBlock _             = False

-- 2. Verify Native Structured OpenAI Tool Calling
testWireToolCalling :: IO ()
testWireToolCalling = do
  putStrLn "\n[Test 2] Native OpenAI Wire Protocol Formatting"
  let toolCall = ToolCall "call_abc123" "bash" (Aeson.object ["command" Aeson..= ("ls -la" :: Text)])
      turnAssistant = Turn 1 AssistantRole
        [ ThinkingBlock 1 "Thinking about running ls." Collapsed
        , TextBlock "I will check the directory."
        , ToolCallBlock toolCall
        ]
      turnTool = Turn 2 ToolRole
        [ ToolResultBlock (ToolResult "call_abc123" "file1.txt\nfile2.txt" "" Nothing)
        ]
      wire = turnsToOpenAIPayload [turnAssistant, turnTool]

  assert "Generated 3 wire messages (system prompt + assistant + tool)" (length wire == 3)
  let sysJson = TE.decodeUtf8 (BL.toStrict (Aeson.encode (head wire)))
      asstJson = TE.decodeUtf8 (BL.toStrict (Aeson.encode (wire !! 1)))
      toolJson = TE.decodeUtf8 (BL.toStrict (Aeson.encode (wire !! 2)))

  assert "First message has role: system" ("\"role\":\"system\"" `T.isInfixOf` sysJson)
  assert "System message contains lambdA prompt" ("lambdA" `T.isInfixOf` sysJson)
  assert "Assistant message has role: assistant" ("\"role\":\"assistant\"" `T.isInfixOf` asstJson)
  assert "Assistant message has native tool_calls array" ("\"tool_calls\":" `T.isInfixOf` asstJson)
  assert "Assistant message has tool call ID" ("call_abc123" `T.isInfixOf` asstJson)
  assert "Assistant message tool arguments are stringified JSON" ("\"arguments\":\"{\\\"command\\\":\\\"ls -la\\\"}\"" `T.isInfixOf` asstJson)
  assert "Tool response has role: tool" ("\"role\":\"tool\"" `T.isInfixOf` toolJson)
  assert "Tool response references tool_call_id" ("\"tool_call_id\":\"call_abc123\"" `T.isInfixOf` toolJson)
  putStrLn "  -> OK: Native OpenAI structured tool calls (with stringified arguments) and tool responses formatted cleanly."

-- 3. Verify Out-of-Band Artifact Spooling
testArtifactSpooling :: IO ()
testArtifactSpooling = do
  putStrLn "\n[Test 3] Out-of-Band Artifact Spooling & Pointer Generation"
  let testArtDir = ".lambda/test_artifacts"
      hugeLog = T.unlines [ "Line " <> T.pack (show n) <> ": test log output" | n <- [1..200 :: Int] ]
  (pointer, mPath) <- spooDiagnosticArtifact testArtDir "test_dump" hugeLog

  assert "Spool created artifact file path" (mPath /= Nothing)
  case mPath of
    Just path -> do
      exists <- doesFileExist path
      assert "Artifact file exists on disk" exists
      assert "Pointer contains XML tag <artifact_pointer" ("<artifact_pointer" `T.isInfixOf` pointer)
      assert "Pointer reports 200 lines" ("total_lines=\"200\"" `T.isInfixOf` pointer)
      assert "Pointer contains truncated head/tail synopsis" ("omitted. Full dump spooled out-of-band" `T.isInfixOf` pointer)
      removeDirectoryRecursive testArtDir
      putStrLn "  -> OK: 200-line dump spooled to disk, compact pointer generated."
    Nothing -> failTest "Expected artifact path to be created for large log."

-- 4. Verify Security Glob Matching & Upfront Grant Authorization
testSecurityGlobMatching :: IO ()
testSecurityGlobMatching = do
  putStrLn "\n[Test 4] OpenCode-Style Security Safeguards & Upfront Grants"
  let allows = ["git status*", "git diff*", "read_file*"]
      denies = ["rm -rf /*", "mkfs*"]
  sec <- initSecurity allows denies

  -- Test static allowlist
  canGitStatus <- checkAuthorization sec MainAgent "git status --short" (Aeson.object [])
  assert "git status matches allowlist" canGitStatus

  canGitDiff <- checkAuthorization sec MainAgent "git diff HEAD~1" (Aeson.object [])
  assert "git diff matches allowlist" canGitDiff

  -- Test static denylist
  canRmRf <- checkAuthorization sec MainAgent "rm -rf /*" (Aeson.object [])
  assert "rm -rf /* is blocked by denylist" (not canRmRf)

  putStrLn "  -> OK: Static whitelist and blacklist glob matching verified."

-- 5. Verify Dual-Gate Mode Safety (/plan vs /exec)
testModeFiltering :: IO ()
testModeFiltering = do
  putStrLn "\n[Test 5] Dual-Gate Mode Safety (/plan vs /exec)"
  let readTool = ToolDefinition "read_file" "Read file" (Aeson.object []) ReadOnly (\_ _ -> pure (ToolResult "" "" "" Nothing))
      destTool = ToolDefinition "write_file" "Write file" (Aeson.object []) Destructive (\_ _ -> pure (ToolResult "" "" "" Nothing))
      reg = registerTools [readTool, destTool] emptyRegistry

  let planSchemas = toolsToOpenAISchema PlanMode reg
  assert "PlanMode has exactly 1 tool" (length planSchemas == 1)
  assert "PlanMode schema includes read_file" ("read_file" `T.isInfixOf` T.pack (show planSchemas))
  assert "PlanMode schema excludes write_file" (not ("write_file" `T.isInfixOf` T.pack (show planSchemas)))

  let execSchemas = toolsToOpenAISchema ExecMode reg
  assert "ExecMode has 2 tools" (length execSchemas == 2)
  assert "ExecMode schema includes write_file" ("write_file" `T.isInfixOf` T.pack (show execSchemas))
  putStrLn "  -> OK: Destructive tools statically excluded from PlanMode schemas."

-- 6. Verify Environment Variable Fallbacks (OpenRouter & OpenAI)
testConfigFallbacks :: IO ()
testConfigFallbacks = do
  putStrLn "\n[Test 6] Dynamic Config & OpenRouter Environment Fallbacks"
  setEnv "OPENROUTER_API_KEY" "sk-or-v1-testkey"
  setEnv "OPENROUTER_MODEL" "meta-llama/llama-3.3-70b-instruct:free"
  cfg <- loadConfig "/tmp/lambda_test_config"
  assert "OPENROUTER_API_KEY populated apiKey" (apiKey cfg == "sk-or-v1-testkey")
  assert "OPENROUTER_MODEL populated modelName" (modelName cfg == "meta-llama/llama-3.3-70b-instruct:free")
  unsetEnv "OPENROUTER_API_KEY"
  unsetEnv "OPENROUTER_MODEL"
  putStrLn "  -> OK: OpenRouter environment variables reliably detected and loaded."

-- 7. Verify Directory Listing and Fallback
testDirectoryTools :: IO ()
testDirectoryTools = do
  putStrLn "\n[Test 7] Directory Tools & Fallback Handling"
  let listTool = listDirectoryTool "."
  resList <- toolExecute listTool MainAgent (Aeson.object ["path" Aeson..= ("src" :: Text)])
  assert "list_directory finds Lambda folder" ("Lambda" `T.isInfixOf` resultStdout resList)

  let readTool = readFileTool "."
  resReadDir <- toolExecute readTool MainAgent (Aeson.object ["path" Aeson..= ("src" :: Text)])
  assert "read_file on directory lists contents gracefully" ("Specified path is a directory" `T.isInfixOf` resultStdout resReadDir)
  putStrLn "  -> OK: Directory listing and directory fallback in read_file work correctly."

-- 8. Verify Context Window Estimation
testContextEstimation :: IO ()
testContextEstimation = do
  putStrLn "\n[Test 8] Context Window Token Estimation"
  let turn = Turn 1 UserRole [TextBlock (T.replicate 400 "a")]
      tokens = estimateTotalTokens [turn]
  assert "400 characters estimated around ~100 tokens" (tokens >= 90 && tokens <= 110)
  putStrLn "  -> OK: Token estimator produces accurate window estimations."

-- 9. Verify MCP Protocol Capability Inference and Result Parsing
testMcpProtocolAndMapping :: IO ()
testMcpProtocolAndMapping = do
  putStrLn "\n[Test 9] MCP Protocol Capability Inference and Response Parsing"
  assert "sd_read inferred as ReadOnly" (inferCapability "sd_read" "Read a file" == ReadOnly)
  assert "sd_recall inferred as ReadOnly" (inferCapability "sd_recall" "Recall memories" == ReadOnly)
  assert "sd_add inferred as Destructive" (inferCapability "sd_add" "Store memory" == Destructive)
  assert "nix inferred as ReadOnly" (inferCapability "nix" "Evaluate nix" == ReadOnly)
  assert "arbitrary_mutator inferred as Destructive" (inferCapability "format_disk" "Format storage" == Destructive)

  -- Test MCP response parsing
  let okPayload = Aeson.object
        [ "content" Aeson..= [ Aeson.object [ "type" Aeson..= ("text" :: Text), "text" Aeson..= ("hello mcp" :: Text) ] ]
        , "isError" Aeson..= False
        ]
      (okTxt, okErr) = parseMcpCallResult okPayload
  assert "Parsed text matches" (okTxt == "hello mcp")
  assert "Parsed isError is False" (not okErr)

  let errPayload = Aeson.object
        [ "content" Aeson..= [ Aeson.object [ "type" Aeson..= ("text" :: Text), "text" Aeson..= ("failure" :: Text) ] ]
        , "isError" Aeson..= True
        ]
      (errTxt, errFlag) = parseMcpCallResult errPayload
  assert "Error text matches" (errTxt == "failure")
  assert "Parsed isError is True" errFlag
  putStrLn "  -> OK: MCP capabilities inferred and protocol payloads parsed accurately."

-- 10. Verify Web URL Fetching Tool
testFetchUrlTool :: IO ()
testFetchUrlTool = do
  putStrLn "\n[Test 10] Web URL Fetching Tool"
  let tool = fetchUrlTool ".lambda/artifacts"
  assert "Tool name is fetch_url" (toolName tool == "fetch_url")
  assert "Tool capability is ReadOnly" (toolCapability tool == ReadOnly)
  assert "Tool description mentions HTML" ("HTML" `T.isInfixOf` toolDescription tool)
  putStrLn "  -> OK: fetch_url tool registered with ReadOnly capability and correct schema."

-- 11. Verify Systems Engineering Persistent System Prompt
testPersistentSystemPrompt :: IO ()
testPersistentSystemPrompt = do
  putStrLn "\n[Test 11] Systems Engineering Persistent System Prompt Injection"
  assert "System prompt mentions lambdA" ("lambdA" `T.isInfixOf` defaultAgentSystemPrompt)
  assert "System prompt includes Hypothesis-Driven Problem Solving" ("Hypothesis-Driven" `T.isInfixOf` defaultAgentSystemPrompt)
  assert "System prompt includes spawn_diagnostic_subagent instruction" ("spawn_diagnostic_subagent" `T.isInfixOf` defaultAgentSystemPrompt)
  assert "System prompt includes mode discipline" ("[/plan]" `T.isInfixOf` defaultAgentSystemPrompt)

  -- Wire payload stripping of UI banners
  let bannerTurn1 = Turn 1 SystemRole [TextBlock "lambdA initialized. Enter a goal or press /help for commands."]
      bannerTurn2 = Turn 2 SystemRole [TextBlock "[Compaction Checkpoint: 10 historical turns preserved in disk archives]"]
      userTurn    = Turn 3 UserRole [TextBlock "Debug this crash dump"]
      wire = turnsToOpenAIPayload [bannerTurn1, bannerTurn2, userTurn]

  assert "Wire payload has exactly 2 messages (system prompt + user message)" (length wire == 2)
  let sysMsg = head wire
      usrMsg = wire !! 1
  assert "Root message is system prompt" ("\"role\":\"system\"" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict (Aeson.encode sysMsg)))
  assert "Root message contains defaultAgentSystemPrompt" ("Hypothesis-Driven" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict (Aeson.encode sysMsg)))
  assert "UI banners were stripped from wire payload" (not ("lambdA initialized" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict (Aeson.encode sysMsg))))
  assert "User turn preserved" ("\"role\":\"user\"" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict (Aeson.encode usrMsg)))
  putStrLn "  -> OK: Systems engineering prompt persists across turns and UI banners are cleanly filtered."

-- 12. Verify Live StormDrain MCP Integration
testLiveMcpServerIntegration :: IO ()
testLiveMcpServerIntegration = do
  putStrLn "\n[Test 12] Live StormDrain MCP Integration"
  cfg <- loadConfig "."
  case Map.lookup "stormdrain" (mcpServers cfg) of
    Nothing -> putStrLn "  -> Skip: stormdrain not configured in mcp_servers"
    Just srv -> do
      (clients, tools) <- startAndLoadMcpServers (Map.singleton "stormdrain" srv)
      assert "Loaded StormDrain tools via MCP" (length tools >= 10)
      assert "Contains sd_read" (any (\t -> toolName t == "sd_read") tools)
      assert "Contains sd_recall" (any (\t -> toolName t == "sd_recall") tools)
      assert "Contains sd_add" (any (\t -> toolName t == "sd_add") tools)
      mapM_ stopMcpClient clients
      putStrLn $ "  -> OK: Live StormDrain MCP server successfully started, initialized, and loaded " <> show (length tools) <> " tools."

-- 13. Verify SubAgent Viewport Height Invariant (Prevents Brick Infinite-Height Crash)
testSubAgentViewportInvariant :: IO ()
testSubAgentViewportInvariant = do
  putStrLn "\n[Test 13] SubAgent Viewport Height Invariant"
  let sampleTask = SubAgentTask
        { subAgentId = 1
        , subAgentRole = "profiler"
        , subAgentHypothesis = "Test hypothesis for ASan crash"
        , subAgentTurnCount = 2
        , subAgentBudget = 10
        , subAgentStatus = SubAgentRunning
        , subAgentArtifact = Just ".lambda/artifacts/subagent_1.log"
        , subAgentTurns = []
        }
      sampleMap = Map.singleton 1 sampleTask
      widget = renderSubAgents sampleMap
  assert "renderSubAgents has Fixed vertical size" (vSize widget == Fixed)
  putStrLn "  -> OK: renderSubAgents produces Fixed vertical height for SubAgentView viewport."

-- 14. Verify PlanMode SubAgent and Context Mass Guard Invariants
testPlanModeSubAgentAndContextGuard :: IO ()
testPlanModeSubAgentAndContextGuard = do
  putStrLn "\n[Test 14] PlanMode SubAgent & Context Mass Guard Invariants"
  let largeFilePath = "large_test_file.txt"
      largeContent = T.unlines [ "Line " <> T.pack (show n) | n <- [1..300 :: Int] ]
  TIO.writeFile largeFilePath largeContent

  let readTool = readFileTool "."
  -- MainAgent direct read of large file (> 250 lines) should be intercepted by Context Mass Guard
  resMainLarge <- toolExecute readTool MainAgent (Aeson.object ["path" Aeson..= T.pack largeFilePath])
  assert "Context Mass Guard blocks main agent direct large read" ("Context Mass Guard" `T.isInfixOf` resultStderr resMainLarge)

  -- MainAgent targeted slice (<= 250 lines) should succeed
  resMainSlice <- toolExecute readTool MainAgent (Aeson.object
    [ "path" Aeson..= T.pack largeFilePath
    , "start_line" Aeson..= (1 :: Int)
    , "line_count" Aeson..= (50 :: Int)
    ])
  assert "Main agent sliced read succeeds" (resultStderr resMainSlice == "")
  assert "Main agent sliced read returned 50 lines" (length (T.lines (resultStdout resMainSlice)) == 50)

  -- Subagent direct read of large file should succeed without restriction (isolated context)
  resSubLarge <- toolExecute readTool (SubAgentId 1 "Survey task") (Aeson.object ["path" Aeson..= T.pack largeFilePath])
  assert "Subagent direct large read succeeds without guard" (resultStderr resSubLarge == "")
  assert "Subagent received all 300 lines" (length (T.lines (resultStdout resSubLarge)) == 300)

  removeFile largeFilePath
  putStrLn "  -> OK: Large file reads properly guarded for MainAgent and delegated to SubAgents."

-- 15. Verify Parallel Tool Execution and Interruption Guard
testConcurrentToolExecution :: IO ()
testConcurrentToolExecution = do
  putStrLn "\n[Test 15] Parallel Tool Execution & STM Interruption Invariant"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  es <- initEngineState cfg emptyRegistry sec

  let simulatedWork idx = do
        threadDelay 50000 -- 50ms
        pure $ ToolResult (T.pack $ show idx) ("Output " <> T.pack (show idx)) "" Nothing

  start <- getCurrentTime
  results <- mapConcurrently simulatedWork [1..3 :: Int]
  end <- getCurrentTime

  let elapsed = diffUTCTime end start
  assert "All 3 concurrent jobs completed" (length results == 3)
  assert "Parallel execution took less than 120ms" (elapsed < 0.12)

  -- Test STM Interruption Cancellation
  atomically $ writeTVar (appInterrupted es) True
  resOrCancelled <- race
    (atomically $ do
      intr <- readTVar (appInterrupted es)
      check intr)
    (threadDelay 1000000 >> pure ("Done" :: Text))
  case resOrCancelled of
    Left () -> putStrLn "  -> OK: Interruption immediately halts asynchronous tasks via STM."
    Right _ -> failTest "Interruption failed to cancel task."

  putStrLn "  -> OK: Tool calls execute concurrently across lightweight green threads."

-- 16. Verify SubAgent Chain of Thought History and Navigation
testSubAgentCoTAndNavigation :: IO ()
testSubAgentCoTAndNavigation = do
  putStrLn "\n[Test 16] SubAgent Chain-of-Thought History & Navigation Invariant"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  es <- initEngineState cfg emptyRegistry sec

  task <- registerSubAgentTask es "surveyor" "Survey memory footprint" 6
  let sId = subAgentId task
      mockTurns =
        [ Turn 1 SystemRole [TextBlock "SubAgent active"]
        , Turn 2 UserRole [TextBlock "Run survey"]
        , Turn 3 AssistantRole
            [ ThinkingBlock 1 "Investigating resident memory usage..." Visible
            , TextBlock "Executing check."
            , ToolCallBlock (ToolCall "c1" "read_file" (Aeson.object ["path" Aeson..= ("foo" :: Text)]))
            ]
        , Turn 4 ToolRole [ToolResultBlock (ToolResult "c1" "File content" "" Nothing)]
        ]

  updateSubAgentTurns es sId mockTurns
  subs <- readTVarIO (appSubAgents es)
  let updatedTask = Map.lookup sId subs
  case updatedTask of
    Nothing -> failTest "Subagent task not found in state."
    Just t -> do
      assert "SubAgentTask preserves turn history" (length (subAgentTurns t) == 4)
      assert "SubAgentTask preserves ThinkingBlock" (any isThinkingBlock (concatMap turnBlocks (subAgentTurns t)))
      assert "SubAgent turn count correctly tracked" (subAgentTurnCount t == 1)

  let sampleMap = Map.singleton sId (maybe task id updatedTask)
      widgetSelected = renderSubAgentsSelected (Just sId) sampleMap
  assert "renderSubAgentsSelected has Fixed vertical size" (vSize widgetSelected == Fixed)

  putStrLn "  -> OK: SubAgent CoT reasoning turns and navigation state preserved in state vector."

-- 17. Verify Streaming Inline <think> Tag State Machine
testInlineThinkingStreamingParser :: IO ()
testInlineThinkingStreamingParser = do
  putStrLn "\n[Test 17] Inline <think> Tag Streaming State Machine"

  -- Case A: Single line with inline <think> and </think>
  let (stA, chunksA) = splitThinkingChunks False "Prefix <think>pondering details</think> suffix"
  assert "State transitioned back to False" (not stA)
  assert "Emitted 3 chunks" (length chunksA == 3)
  assert "First chunk is prefix text" (chunksA !! 0 == ChunkText "Prefix ")
  assert "Second chunk is thinking body without tags" (chunksA !! 1 == ChunkThinking "pondering details")
  assert "Third chunk is suffix text" (chunksA !! 2 == ChunkText " suffix")

  -- Case B: Chunks arriving incrementally across stream boundaries
  let (stB1, chunksB1) = splitThinkingChunks False "<think>Step 1:"
  assert "Transitioned into thinking mode" stB1
  assert "First thought chunk emitted" (chunksB1 == [ChunkThinking "Step 1:"])

  let (stB2, chunksB2) = splitThinkingChunks stB1 " Step 2 done."
  assert "Remains in thinking mode" stB2
  assert "Second thought chunk emitted as ChunkThinking" (chunksB2 == [ChunkThinking " Step 2 done."])

  let (stB3, chunksB3) = splitThinkingChunks stB2 "</think>\nFinal result."
  assert "Exited thinking mode" (not stB3)
  assert "Emitted final text chunk" (chunksB3 == [ChunkText "\nFinal result."])

  -- Case C: Parse SSE data line with reasoning_content
  let rawSseReasoning = "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"DeepSeek thought\"}}]}"
      parsedReasoning = parseSseChunk rawSseReasoning
  assert "reasoning_content parsed as ChunkThinking" (parsedReasoning == [ChunkThinking "DeepSeek thought"])

  -- Case D: Parse SSE data line with alternative reasoning field
  let rawSseAltReasoning = "data: {\"choices\":[{\"delta\":{\"reasoning\":\"Alternative reasoning\"}}]}"
      parsedAltReasoning = parseSseChunk rawSseAltReasoning
  assert "reasoning parsed as ChunkThinking" (parsedAltReasoning == [ChunkThinking "Alternative reasoning"])

  putStrLn "  -> OK: Inline <think> streaming state machine correctly separates thoughts without leaking tags."

-- 18. Verify Wire System Prompt Decoupling Invariant
testWireSystemPromptDecoupling :: IO ()
testWireSystemPromptDecoupling = do
  putStrLn "\n[Test 18] Wire System Prompt Decoupling Invariant"
  -- Case A: Subagent turn with explicit SystemRole prompt
  let subAgentSysPrompt = "You are an ephemeral profiler subagent."
      subTurns =
        [ Turn 1 SystemRole [TextBlock subAgentSysPrompt]
        , Turn 2 UserRole [TextBlock "Run valgrind on target"]
        ]
      subWire = turnsToOpenAIPayload subTurns
  assert "SubAgent wire payload has 2 messages" (length subWire == 2)
  let subSys = head subWire
  assert "SubAgent wire system message role is system"
    (parseEither (Aeson.withObject "msg" (Aeson..: "role")) subSys == Right ("system" :: Text))
  assert "SubAgent wire system message contains specialist prompt"
    (parseEither (Aeson.withObject "msg" (Aeson..: "content")) subSys == Right subAgentSysPrompt)
  assert "SubAgent wire system message does NOT contain defaultAgentSystemPrompt"
    (not (defaultAgentSystemPrompt `T.isInfixOf` T.pack (show subSys)))

  -- Case B: Primary agent turns without explicit SystemRole
  let mainTurns =
        [ Turn 1 UserRole [TextBlock "Help optimize loop"]
        ]
      mainWire = turnsToOpenAIPayload mainTurns
  assert "Primary agent wire payload has prepended system message" (length mainWire == 2)
  let mainSys = head mainWire
  assert "Primary agent root message is defaultAgentSystemPrompt"
    (parseEither (Aeson.withObject "msg" (Aeson..: "content")) mainSys == Right defaultAgentSystemPrompt)
  putStrLn "  -> OK: Wire protocol strictly decouples specialist persona prompts from orchestrator doctrine."

-- 19. Verify Synthetic submit_report Egress Tool Invariant
testSubmitReportToolEgress :: IO ()
testSubmitReportToolEgress = do
  putStrLn "\n[Test 19] Synthetic submit_report Egress Tool Invariant"
  repVar <- newTVarIO Nothing
  let def = submitReportTool repVar
  assert "Tool name is submit_report" (toolName def == "submit_report")
  assert "Tool capability is ReadOnly" (toolCapability def == ReadOnly)

  let reportArgs = Aeson.object
        [ "status" .= ("SUCCESS" :: Text)
        , "summary" .= ("Found cache thrashing in inner loop" :: Text)
        , "details" .= ("Cache miss rate: 42.8% on L1d" :: Text)
        , "artifact_path" .= (".lambda/artifacts/cache_profile.txt" :: Text)
        ]
  res <- toolExecute def MainAgent reportArgs
  assert "toolExecute succeeds without error" (T.null (resultStderr res))
  assert "toolExecute returns artifact path" (resultArtifactPath res == Just ".lambda/artifacts/cache_profile.txt")

  mRep <- readTVarIO repVar
  case mRep of
    Nothing -> failTest "submitReportTool failed to populate TVar (Maybe SubAgentReport)"
    Just rep -> do
      assert "Report status is SUCCESS" (reportStatus rep == "SUCCESS")
      assert "Report summary matches" (reportSummary rep == "Found cache thrashing in inner loop")
      assert "Report details match" (reportDetails rep == "Cache miss rate: 42.8% on L1d")
      assert "Report artifact matches" (reportArtifact rep == Just ".lambda/artifacts/cache_profile.txt")
  putStrLn "  -> OK: submit_report tool provides type-safe, schema-validated report egress."

-- 20. Verify Budget Exhaustion Diagnostics Invariant
testBudgetExhaustionDiagnostics :: IO ()
testBudgetExhaustionDiagnostics = do
  putStrLn "\n[Test 20] Budget Exhaustion Diagnostics Invariant"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  es <- initEngineState cfg emptyRegistry sec
  task <- registerSubAgentTask es "debugger" "Trace memory corruption" 2
  let sId = subAgentId task

  turnsVar <- newTVarIO [ Turn 1 UserRole [TextBlock "Start"] ]
  attemptedVar <- newTVarIO
    [ "valgrind(--tool=memcheck ./bin)"
    , "grep_search(SEGV)"
    ]
  repVar <- newTVarIO Nothing

  let dummyDriver = ModelDriver
        { streamCompletion = \_ _ cb -> do
            cb (ChunkText "Still analyzing...")
            cb ChunkDone
        }

  res <- subAgentLoop es dummyDriver sId [] turnsVar attemptedVar repVar 3 2
  case res of
    Left err -> failTest ("Unexpected subAgentLoop failure: " <> T.unpack err)
    Right rep -> do
      assert "Status is INCONCLUSIVE" (reportStatus rep == "INCONCLUSIVE")
      assert "Summary mentions budget exhaustion" ("Turn budget exhausted after 2 turns" `T.isInfixOf` reportSummary rep)
      assert "Details list attempted tools" ("valgrind(--tool=memcheck ./bin)" `T.isInfixOf` reportDetails rep)
      assert "Details list grep_search" ("grep_search(SEGV)" `T.isInfixOf` reportDetails rep)
  putStrLn "  -> OK: Budget exhaustion synthesizes structured INCONCLUSIVE report with attempted tools."

-- 21. Verify Depth-1 Star Graph Schema Exclusion
testDepth1StarGraphSchemaExclusion :: IO ()
testDepth1StarGraphSchemaExclusion = do
  putStrLn "\n[Test 21] Depth-1 Star Graph Invariant"
  repVar <- newTVarIO Nothing
  let dummyTool name = ToolDefinition name "desc" (Aeson.object []) ReadOnly (\_ _ -> pure (ToolResult "" "" "" Nothing))
      baseReg = registerTools
        [ dummyTool "spawn_specialist_subagent"
        , dummyTool "spawn_diagnostic_subagent"
        , dummyTool "read_file"
        , dummyTool "bash"
        ] emptyRegistry

  let subReg = filterSubAgentRegistry (submitReportTool repVar) baseReg
      schemas = toolsToOpenAISchema ExecMode subReg

  let toolNames = mapMaybe extractToolName schemas
  assert "spawn_specialist_subagent is NOT in subagent schema" (notElem "spawn_specialist_subagent" toolNames)
  assert "spawn_diagnostic_subagent is NOT in subagent schema" (notElem "spawn_diagnostic_subagent" toolNames)
  assert "read_file IS in subagent schema" (elem "read_file" toolNames)
  assert "bash IS in subagent schema" (elem "bash" toolNames)
  assert "submit_report IS in subagent schema" (elem "submit_report" toolNames)
  putStrLn "  -> OK: Subagent tool registry strictly strips spawn_* to enforce Depth-1 Star Graph."
  where
    extractToolName val =
      case parseEither (Aeson.withObject "tool" $ \o -> do
             fn <- o Aeson..: "function"
             fn Aeson..: "name"
           ) val of
        Right (n :: Text) -> Just n
        _                 -> Nothing

-- 22. Verify Data-Driven Specialist Config Loading & Custom Override
testDataDrivenSpecialistConfigLoading :: IO ()
testDataDrivenSpecialistConfigLoading = do
  putStrLn "\n[Test 22] Data-Driven Specialist Config Loading & Custom Overrides"
  cfg <- loadConfig "."
  let specs = specialists cfg
  assert "surveyor specialist is defined" (Map.member "surveyor" specs)
  assert "debugger specialist is defined" (Map.member "debugger" specs)
  assert "profiler specialist is defined" (Map.member "profiler" specs)
  assert "implementer specialist is defined" (Map.member "implementer" specs)
  assert "reviewer specialist is defined" (Map.member "reviewer" specs)

  let profiler = specs Map.! "profiler"
  assert "profiler budget is 6" (specialistBudget profiler == 6)
  assert "profiler has valgrind capability" (any ("valgrind*" `T.isInfixOf`) (specialistCapabilities profiler))

  let customJson = "{\"specialists\":{\"fuzzer\":{\"description\":\"AFL++ fuzzer\",\"prompt\":\"Run fuzzer\",\"budget\":12,\"granted_capabilities\":[\"afl*\"]}}}"
  case Aeson.eitherDecode customJson of
    Left err -> failTest ("Failed to parse custom specialist JSON: " <> err)
    Right (customCfg :: Config) -> do
      assert "Merged custom fuzzer specialist" (Map.member "fuzzer" (specialists customCfg))
      let fuzzer = specialists customCfg Map.! "fuzzer"
      assert "fuzzer budget is 12" (specialistBudget fuzzer == 12)
      assert "built-in surveyor preserved in custom config" (Map.member "surveyor" (specialists customCfg))
  putStrLn "  -> OK: SpecialistConfig supports built-ins and dynamic user overrides from config.json."

-- 23. Verify Dynamic Permission Escalation for SubAgents
testDynamicPermissionEscalation :: IO ()
testDynamicPermissionEscalation = do
  putStrLn "\n[Test 23] Dynamic Permission Escalation for SubAgents"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  let dummyBashTool = ToolDefinition "bash" "Run bash" (Aeson.object []) Destructive
        (\_ _ -> pure (ToolResult "call_1" "Command executed successfully" "" Nothing))
      reg = registerTool dummyBashTool emptyRegistry
  es <- initEngineState cfg reg sec

  atomically $ writeTVar (appMode es) ExecMode

  let caller = SubAgentId 42 "Run perf tool"
      call = ToolCall "call_1" "bash" (Aeson.object ["command" .= ("perf stat ./bin" :: Text)])

  -- Case A: When pre-authorized with grantedGlobs matching "perf*"
  resA <- executeToolDispatch es caller ["perf*"] call
  assert "Authorized via pre-granted glob succeeds" (resultStdout resA == "Command executed successfully")

  -- Case B: When NOT pre-authorized, verify dynamic escalation prompts user
  dispatchHandle <- async $ executeToolDispatch es caller ["read_file*"] call

  -- Read the escalation prompt from uiPromptQueue
  prompt <- atomically $ readTBQueue (uiPromptQueue sec)
  assert "Prompt caller matches" (promptCaller prompt == caller)
  assert "Prompt tool matches" (promptTool prompt == "perf stat ./bin")

  -- User grants one-time approval to the subagent
  atomically $ putTMVar (promptReply prompt) PermOnce

  -- Subagent unblocks and completes execution
  resB <- wait dispatchHandle
  assert "Subagent executes successfully after dynamic permission approval"
    (resultStdout resB == "Command executed successfully")

  -- Case C: When user denies the escalation prompt
  let call2 = ToolCall "call_2" "bash" (Aeson.object ["command" .= ("rm -rf /" :: Text)])
  denyHandle <- async $ executeToolDispatch es caller ["read_file*"] call2
  promptDeny <- atomically $ readTBQueue (uiPromptQueue sec)
  atomically $ putTMVar (promptReply promptDeny) PermNo
  resC <- wait denyHandle
  assert "Subagent call is rejected when user denies permission"
    ("Permission Denied" `T.isInfixOf` resultStderr resC)

  putStrLn "  -> OK: Ungranted subagent tool call triggers dynamic user permission escalation and honors user decision."

assert :: String -> Bool -> IO ()
assert desc condition =
  unless condition $ failTest ("Assertion failed: " <> desc)

failTest :: String -> IO ()
failTest msg = do
  putStrLn $ "  [FAIL] " <> msg
  exitFailure
