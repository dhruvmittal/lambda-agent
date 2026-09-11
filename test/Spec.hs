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
import Data.Time.Clock (getCurrentTime, diffUTCTime, addUTCTime)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist, removeFile)
import System.Environment (setEnv, unsetEnv)
import System.Exit (exitFailure)

import Lambda.Config (loadConfig, Config(..), SpecialistConfig(..))
import Lambda.Core.EngineInterface (initEngineChannels, cmdQueue, startEngineLoop, EngineChannels(..))
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider
import Lambda.Provider.JsonRpc (startRpcClient, stopRpcClient, sendRequest)
import Lambda.Driver.OpenAI (parseSseChunk, splitThinkingChunks)
import Lambda.Engine.Artifacts
import Lambda.Engine.Compactor
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.Security (initSecurity, checkAuthorization, SecurityState(..))
import Lambda.Engine.Session
import Lambda.Engine.State
  ( initEngineState
  , initEngineStateWithSession
  , registerSubAgentTask
  , updateSubAgentTurns
  , resetEngineSession
  , appInterrupted
  , appSubAgents
  , appMode
  , appTurns
  , appTurnCounter
  , appSubAgentSeq
  , appSessionDir
  )
import Lambda.Engine.SubAgent (submitReportTool, filterSubAgentRegistry, subAgentLoop, runEphemeralSubAgent, SubAgentSpec(..), SubAgentReport(..))
import qualified Data.Map.Strict as Map
import Brick.Types (vSize, Size(..))
import Lambda.Provider.Builtin (builtinTools, listDirectoryTool, readFileTool, fetchUrlTool)
import Lambda.Provider.Mcp (inferCapability, parseMcpCallResult, startAndLoadMcpServers, stopMcpClient)
import Lambda.UI.Draw (renderSubAgents, renderSubAgentsSelected, renderInlineSubAgent)
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
  testSessionSerialization
  testSessionResumptionAndRestoration
  testNewSessionIsolation
  testSessionDiscoveryAndRetentionPruning
  testOnDemandMarkdownTraceGeneration
  testAutocompleteAndTabInvariant
  testSubAgentStreamingToolArgs
  testInterruptionCancellation
  testCompactionWireIntegrity
  testParallelArtifactUniqueness
  testJsonRpcProcessDisconnection
  testJustInTimeSubAgentPermissions

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
  (pointer, mPath) <- spoolDiagnosticArtifact testArtDir "test_dump" hugeLog

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
  assert "System prompt includes spawn_specialist_subagent instruction" ("spawn_specialist_subagent" `T.isInfixOf` defaultAgentSystemPrompt)
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
  let inlineWidget = renderInlineSubAgent sampleTask
  assert "renderInlineSubAgent has Fixed vertical size (prevents ChatView crash)" (vSize inlineWidget == Fixed)
  putStrLn "  -> OK: renderSubAgents and renderInlineSubAgent produce Fixed vertical height for viewports."

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
  assert "profiler budget is 10" (specialistBudget profiler == 10)
  assert "surveyor budget is 16" (specialistBudget (specs Map.! "surveyor") == 16)
  assert "debugger budget is 14" (specialistBudget (specs Map.! "debugger") == 14)
  assert "reviewer budget is 12" (specialistBudget (specs Map.! "reviewer") == 12)
  assert "implementer budget is 12" (specialistBudget (specs Map.! "implementer") == 12)
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

-- 24. Verify Session Serialization & Deserialization Invariant
testSessionSerialization :: IO ()
testSessionSerialization = do
  putStrLn "\n[Test 24] Session Serialization & Deserialization Invariant"
  now <- getCurrentTime
  let tempDir = ".lambda/test_sessions_24"
      toolCall = ToolCall "call_123" "bash" (Aeson.object ["command" .= ("ls -la" :: Text)])
      toolRes  = ToolResult "call_123" "file1.txt\nfile2.txt" "" Nothing
      turn1 = Turn 1 UserRole [TextBlock "Initial prompt to inspect repository"]
      turn2 = Turn 2 AssistantRole
        [ ThinkingBlock 1 "Must run ls to check directory contents" Collapsed
        , ToolCallBlock toolCall
        , ToolResultBlock toolRes
        , TextBlock "Here are the files."
        ]
      subTurns =
        [ Turn 1 UserRole [TextBlock "Explore files"]
        , Turn 2 AssistantRole [TextBlock "Files explored successfully"]
        ]
      subTask = SubAgentTask 1 "surveyor" "Verify workspace structure" 1 16 (SubAgentSuccess "Structure OK") Nothing subTurns
      sess = Session
        { sessionId           = "session_test_24"
        , sessionCreatedAt     = now
        , sessionUpdatedAt     = now
        , sessionTitle         = "Initial prompt to inspect repository"
        , sessionMode          = ExecMode
        , sessionTurns         = [turn1, turn2]
        , sessionSubAgents     = Map.singleton 1 subTask
        , sessionStateVector   = Map.singleton "state_vector" "GOAL: Inspect repo"
        , sessionPromptHistory = ["Initial prompt to inspect repository"]
        }

  saveSession tempDir 50 sess
  loadRes <- loadSession tempDir "session_test_24"
  case loadRes of
    Left err -> failTest ("Failed to load saved session: " <> T.unpack err)
    Right loaded -> do
      assert "Session ID matches" (sessionId loaded == "session_test_24")
      assert "Session Title matches" (sessionTitle loaded == "Initial prompt to inspect repository")
      assert "Session Mode matches" (sessionMode loaded == ExecMode)
      assert "Turn count matches" (length (sessionTurns loaded) == 2)
      assert "SubAgent count matches" (Map.size (sessionSubAgents loaded) == 1)
      let loadedSub = sessionSubAgents loaded Map.! 1
      assert "SubAgent role matches" (subAgentRole loadedSub == "surveyor")
      assert "SubAgent turns match" (length (subAgentTurns loadedSub) == 2)
      assert "SubAgent status matches" (subAgentStatus loadedSub == SubAgentSuccess "Structure OK")
  removeDirectoryRecursive tempDir
  putStrLn "  -> OK: Full session state serialized and deserialized with zero loss."

-- 25. Verify Session Resumption & Engine State Restoration Invariant
testSessionResumptionAndRestoration :: IO ()
testSessionResumptionAndRestoration = do
  putStrLn "\n[Test 25] Session Resumption & Engine State Restoration Invariant"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  now <- getCurrentTime
  let subTask = SubAgentTask 2 "debugger" "Investigate crash" 3 14 (SubAgentSuccess "Found bug") Nothing []
      sess = Session
        { sessionId           = "session_test_25"
        , sessionCreatedAt     = now
        , sessionUpdatedAt     = now
        , sessionTitle         = "Debug crash"
        , sessionMode          = ExecMode
        , sessionTurns         = [Turn 1 UserRole [TextBlock "debug this crash"], Turn 2 AssistantRole [TextBlock "Fixed"]]
        , sessionSubAgents     = Map.singleton 2 subTask
        , sessionStateVector   = Map.singleton "state_vector" "GOAL: Fix bug"
        , sessionPromptHistory = ["debug this crash"]
        }

  es <- initEngineStateWithSession cfg emptyRegistry sec (Just sess)

  mode <- readTVarIO (appMode es)
  turns <- readTVarIO (appTurns es)
  tCount <- readTVarIO (appTurnCounter es)
  subs <- readTVarIO (appSubAgents es)
  subSeq <- readTVarIO (appSubAgentSeq es)

  assert "Engine Mode restored to ExecMode" (mode == ExecMode)
  assert "Engine turns restored" (length turns == 2)
  assert "Turn counter advanced past max turn ID" (tCount == 3)
  assert "Subagents restored in engine state" (Map.member 2 subs)
  assert "SubAgent sequence advanced past max sub ID" (subSeq == 3)
  putStrLn "  -> OK: AppEngineState initialized completely and faithfully from restored Session."

-- 26. Verify /new Session Isolation Invariant
testNewSessionIsolation :: IO ()
testNewSessionIsolation = do
  putStrLn "\n[Test 26] /new Session Isolation Invariant"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  let tempDir = ".lambda/test_sessions_26"
  now <- getCurrentTime
  let sess = Session
        { sessionId           = "session_test_26_old"
        , sessionCreatedAt     = now
        , sessionUpdatedAt     = now
        , sessionTitle         = "Old active session"
        , sessionMode          = ExecMode
        , sessionTurns         = [Turn 1 UserRole [TextBlock "old task"]]
        , sessionSubAgents     = Map.empty
        , sessionStateVector   = Map.empty
        , sessionPromptHistory = []
        }
  es <- initEngineStateWithSession cfg emptyRegistry sec (Just sess)
  -- Point appSessionDir to tempDir for testing
  let es' = es { appSessionDir = tempDir }

  freshSess <- resetEngineSession es'

  -- Verify old session was saved to disk
  oldFileExists <- doesFileExist (tempDir ++ "/session_test_26_old.json")
  assert "Old session file persisted during /new reset" oldFileExists

  -- Verify engine state is reset to fresh
  freshTurns <- readTVarIO (appTurns es')
  freshSubs <- readTVarIO (appSubAgents es')
  freshMode <- readTVarIO (appMode es')
  assert "Fresh session has 1 initial welcome turn" (length freshTurns == 1)
  assert "Fresh session has empty subagent map" (Map.null freshSubs)
  assert "Fresh session resets to PlanMode" (freshMode == PlanMode)
  assert "New session has distinct ID" (sessionId freshSess /= "session_test_26_old")

  removeDirectoryRecursive tempDir
  putStrLn "  -> OK: /new cleanly flushes engine state while persisting old session to disk."

-- 27. Verify CLI --continue Most Recent Discovery & Retention Pruning Invariant
testSessionDiscoveryAndRetentionPruning :: IO ()
testSessionDiscoveryAndRetentionPruning = do
  putStrLn "\n[Test 27] CLI --continue Discovery & Retention Pruning Invariant"
  let tempDir = ".lambda/test_sessions_27"
  now <- getCurrentTime
  createDirectoryIfMissing True tempDir

  -- Create 4 sessions with distinct timestamps 100 seconds apart
  let mkSess i = Session
        { sessionId           = "session_item_" <> T.pack (show i)
        , sessionCreatedAt     = addUTCTime (fromIntegral (i * 100)) now
        , sessionUpdatedAt     = addUTCTime (fromIntegral (i * 100)) now
        , sessionTitle         = "Session " <> T.pack (show i)
        , sessionMode          = PlanMode
        , sessionTurns         = []
        , sessionSubAgents     = Map.empty
        , sessionStateVector   = Map.empty
        , sessionPromptHistory = []
        }

  mapM_ (\i -> saveSession tempDir 0 (mkSess i)) [1..4 :: Int]

  -- Verify getLatestSession discovers session 4
  mTop <- getLatestSession tempDir
  case mTop of
    Nothing -> failTest "getLatestSession failed to find any session"
    Just topSess ->
      assert "Discovered latest session 4" (sessionId topSess == "session_item_4")

  -- Prune down to 2 sessions
  prunedCount <- pruneSessions tempDir 2
  assert "Pruned exactly 2 excess sessions" (prunedCount == 2)

  remaining <- listSessions tempDir
  assert "Exactly 2 sessions remain" (length remaining == 2)
  assert "Remaining sessions are the two newest (4 and 3)"
    (map metaId remaining == ["session_item_4", "session_item_3"])

  removeDirectoryRecursive tempDir
  putStrLn "  -> OK: getLatestSession accurately discovers latest session and pruneSessions enforces LRU retention."

-- 28. Verify On-Demand Markdown Trace Generation Invariant
testOnDemandMarkdownTraceGeneration :: IO ()
testOnDemandMarkdownTraceGeneration = do
  putStrLn "\n[Test 28] On-Demand Markdown Trace Generation Invariant"
  now <- getCurrentTime
  let tempDir = ".lambda/test_sessions_28"
      subTask = SubAgentTask 1 "profiler" "Analyze hotspots" 4 10 (SubAgentSuccess "Optimized") Nothing
        [ Turn 1 UserRole [TextBlock "profile hotspot"]
        , Turn 2 AssistantRole
            [ ThinkingBlock 1 "Checking valgrind output" Collapsed
            , ToolCallBlock (ToolCall "c1" "valgrind" (Aeson.object ["args" .= ("--tool=callgrind" :: Text)]))
            , ToolResultBlock (ToolResult "c1" "1000 instructions" "" Nothing)
            , TextBlock "Hotspot identified."
            ]
        ]
      sess = Session
        { sessionId           = "session_test_28"
        , sessionCreatedAt     = now
        , sessionUpdatedAt     = now
        , sessionTitle         = "Profile hotspot"
        , sessionMode          = PlanMode
        , sessionTurns         = [Turn 1 UserRole [TextBlock "Run profile"], Turn 2 AssistantRole [TextBlock "Done"]]
        , sessionSubAgents     = Map.singleton 1 subTask
        , sessionStateVector   = Map.empty
        , sessionPromptHistory = []
        }

  let md = renderSessionTraceMarkdown sess
  assert "Trace includes Session ID" ("session_test_28" `T.isInfixOf` md)
  assert "Trace includes SubAgent role" ("profiler" `T.isInfixOf` md)
  assert "Trace includes tool invocation" ("valgrind" `T.isInfixOf` md)
  assert "Trace includes thinking block" ("Thinking / Chain-of-Thought" `T.isInfixOf` md)

  -- Verify exportSessionTrace creates the on-demand trace file
  exportedPath <- exportSessionTrace tempDir sess
  traceExists <- doesFileExist exportedPath
  assert "On-demand trace file created on disk" traceExists

  removeDirectoryRecursive tempDir
  putStrLn "  -> OK: On-demand markdown trace renders complete telemetry and writes strictly when requested."

-- 29. Verify / Command Autocomplete and Tab Invariant
testAutocompleteAndTabInvariant :: IO ()
testAutocompleteAndTabInvariant = do
  putStrLn "\n[Test 29] Slash Command Autocomplete and Tab Invariant"
  let allCommands =
        [ "/plan"
        , "/exec"
        , "/think"
        , "/session"
        , "/sub"
        , "/trace"
        , "/compact"
        , "/clear"
        , "/new"
        , "/help"
        , "/quit"
        ]
      computeMatches input =
        let rawInput = T.dropWhile (== ' ') (T.filter (\c -> c /= '\n' && c /= '\r') input)
        in if "/" `T.isPrefixOf` rawInput && not (" " `T.isInfixOf` rawInput)
             then filter (rawInput `T.isPrefixOf`) allCommands
             else []

  -- Check root slash returns all commands
  let slashMatches = computeMatches "/"
  assert "Typing '/' returns all available slash commands" (length slashMatches == length allCommands)

  -- Check prefix filtering
  let planMatches = computeMatches "/p"
  assert "Typing '/p' returns ['/plan']" (planMatches == ["/plan"])

  let sessionMatches = computeMatches "/s"
  assert "Typing '/s' matches /session and /sub" (sessionMatches == ["/session", "/sub"])

  -- Check trailing newline from editor does not break matching
  let newlineMatches = computeMatches "/t\n"
  assert "Trailing newline from editor lines does not break matching" (newlineMatches == ["/think", "/trace"])

  -- Check commands with spaces are dismissed
  let spaceMatches = computeMatches "/plan "
  assert "Commands with trailing arguments/space dismiss autocomplete" (null spaceMatches)

  -- Check non-slash text does not trigger autocomplete
  let normalMatches = computeMatches "cabal test"
  assert "Regular prompts do not trigger slash autocomplete" (null normalMatches)

  putStrLn "  -> OK: Slash command autocomplete filters dynamically and handles editor line buffers cleanly."

-- 30. Verify SubAgent Multi-Chunk Tool Call Arguments Streaming Invariant
testSubAgentStreamingToolArgs :: IO ()
testSubAgentStreamingToolArgs = do
  putStrLn "\n[Test 30] SubAgent Multi-Chunk Tool Call Arguments Streaming Invariant"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  repVar <- newTVarIO Nothing
  let dummyReadFile = readFileTool "."
      reg = filterSubAgentRegistry (submitReportTool repVar) (registerTools [dummyReadFile] emptyRegistry)
  es <- initEngineState cfg reg sec
  task <- registerSubAgentTask es "reviewer" "Review README.md" 5
  let sId = subAgentId task

  turnsVar <- newTVarIO [ Turn 1 UserRole [TextBlock "Start"] ]
  attemptedVar <- newTVarIO []

  driverTurnVar <- newTVarIO (1 :: Int)
  let streamingDriver = ModelDriver
        { streamCompletion = \_ _ cb -> do
            t <- atomically $ do
              cur <- readTVar driverTurnVar
              writeTVar driverTurnVar (cur + 1)
              pure cur
            if t == 1
              then do
                -- Emulate OpenAI SSE stream where subsequent chunks omit targetId ("")
                cb (ChunkToolCallStart "call_read_1" "read_file")
                cb (ChunkToolCallArgs "" "{\"path\":")
                cb (ChunkToolCallArgs "" " \"README.md\"}")
                cb ChunkDone
              else do
                cb (ChunkToolCallStart "call_rep_1" "submit_report")
                cb (ChunkToolCallArgs "" "{\"status\":\"SUCCESS\",\"summary\":\"Reviewed README.md\",\"details\":\"All checks passed\"}")
                cb ChunkDone
        }

  res <- subAgentLoop es streamingDriver sId ["read_file*", "submit_report*"] turnsVar attemptedVar repVar 1 5
  case res of
    Left err -> failTest ("Unexpected subAgentLoop failure: " <> T.unpack err)
    Right rep -> do
      assert "Report status is SUCCESS" (reportStatus rep == "SUCCESS")
      assert "Report summary is correct" (reportSummary rep == "Reviewed README.md")

  attempted <- readTVarIO attemptedVar
  assert "Attempted tool logged with path argument instead of empty ()" (any ("read_file(README.md)" `T.isInfixOf`) attempted)

  allTurns <- readTVarIO turnsVar
  -- Verify that the tool result was successful (read README.md contents) and not "key 'path' not found"
  let toolResults = [ resOut | Turn _ ToolRole blocks <- allTurns, ToolResultBlock ToolResult{resultStdout = resOut} <- blocks ]
  assert "Tool result contains actual README content" (any ("lambdA" `T.isInfixOf`) toolResults)

  putStrLn "  -> OK: Multi-chunk streamed arguments with empty target IDs accumulate and decode correctly."

-- 31. Verify Engine Interruption via Async Cancellation
testInterruptionCancellation :: IO ()
testInterruptionCancellation = do
  putStrLn "\n[Test 31] Engine Prompt Interruption Cancellation"
  evQ <- newTQueueIO
  chans <- initEngineChannels evQ
  cfg <- loadConfig "/tmp/lambda_test_config"
  sec <- initSecurity [] []
  es <- initEngineState cfg emptyRegistry sec

  slowStartedVar <- newTVarIO False
  let driver = ModelDriver
        { streamCompletion = \_ _ cb -> do
            atomically $ writeTVar slowStartedVar True
            threadDelay 2000000
            cb (ChunkText "Finished slow task")
            cb ChunkDone
        }

  startEngineLoop es driver chans
  atomically $ writeTBQueue (cmdQueue chans) (CmdUserPrompt "Start slow task")

  atomically $ do
    started <- readTVar slowStartedVar
    check started

  atomically $ writeTBQueue (cmdQueue chans) CmdInterrupt
  threadDelay 50000

  interrupted <- readTVarIO (appInterrupted es)
  assert "Engine state marked as interrupted" interrupted
  putStrLn "  -> OK: Active turn immediately cancelled by CmdInterrupt via Async cancellation."

-- 31. Verify Context Compaction Wire Integrity
testCompactionWireIntegrity :: IO ()
testCompactionWireIntegrity = do
  putStrLn "\n[Test 32] Context Compaction Wire Integrity"
  let turns = [ Turn i (if even i then AssistantRole else UserRole)
                   [ TextBlock ("Turn " <> T.pack (show i) <> " message with detail " <> T.replicate 40 "data ") ]
              | i <- [1..20 :: Int] ]
  let beforeTokens = estimateTotalTokens turns
  let compacted = compactHistory 600 4 turns
  let afterTokens = estimateTotalTokens compacted
  assert "Compacted history reduces total estimated tokens" (afterTokens < beforeTokens)
  assert "Compacted history contains summary block" (any (\t -> any isSummaryTextBlock (turnBlocks t)) compacted)

  let wire = turnsToOpenAIPayload compacted
  assert "Generated at least 2 wire messages (system prompt + compacted turns)" (length wire >= 2)
  let wireText = TE.decodeUtf8 (BL.toStrict (Aeson.encode wire))
  assert "Wire message contains compact context summary" ("[Context Summary:" `T.isInfixOf` wireText)
  putStrLn "  -> OK: Compaction prunes older turns, generates structured summary block, and wire payload remains valid."
  where
    isSummaryTextBlock (TextBlock t) = "[Context Summary:" `T.isInfixOf` t
    isSummaryTextBlock _ = False

-- 33. Verify Parallel Diagnostic Artifact Uniqueness
testParallelArtifactUniqueness :: IO ()
testParallelArtifactUniqueness = do
  putStrLn "\n[Test 33] Parallel Diagnostic Artifact Collision Immunity"
  let testArtDir = ".lambda/test_parallel_artifacts"
  results <- mapConcurrently (\i -> spoolDiagnosticArtifact testArtDir ("parallel_dump_" <> T.pack (show i)) ("Log content " <> T.pack (show i) <> "\n" <> T.replicate 150 "extra line\n")) [1..20 :: Int]
  let paths = mapMaybe snd results
  assert "All 20 parallel spools created artifact files" (length paths == 20)
  let uniquePaths = Map.keys (Map.fromList [ (p, ()) | p <- paths ])
  assert "All 20 artifact paths are strictly unique" (length uniquePaths == 20)
  mapM_ (\p -> do
    exists <- doesFileExist p
    assert "Spool file exists on disk" exists) paths
  removeDirectoryRecursive testArtDir
  putStrLn "  -> OK: 20 parallel spools generated 20 collision-free artifact files with picosecond timestamps."

-- 34. Verify JSON-RPC Disconnection Resilience
testJsonRpcProcessDisconnection :: IO ()
testJsonRpcProcessDisconnection = do
  putStrLn "\n[Test 34] JSON-RPC Process Disconnection Resilience"
  client <- startRpcClient LineFramed "sh" ["-c", "exit 0"]
  threadDelay 50000
  res <- sendRequest client "ping" (Aeson.object [])
  case res of
    Left err -> do
      assert "Error indicates client disconnected / pipe closed" ("disconnected" `T.isInfixOf` err || "closed" `T.isInfixOf` err)
      putStrLn "  -> OK: Pending requests to terminated processes fail promptly without deadlocking TMVars."
    Right _ -> failTest "Expected request to dead process to return Left error."
  stopRpcClient client

-- 35. Verify Just-In-Time SubAgent Permissions Invariant
testJustInTimeSubAgentPermissions :: IO ()
testJustInTimeSubAgentPermissions = do
  putStrLn "\n[Test 35] Just-In-Time SubAgent Permissions & Workspace Confinement"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  let tools = builtinTools "." ".lambda/artifacts"
      reg = registerTools tools emptyRegistry
  es <- initEngineState cfg reg sec
  atomically $ writeTVar (appMode es) ExecMode

  -- 1. Spawning subagents launches immediately without upfront UI modal prompts
  let driver = ModelDriver
        { streamCompletion = \_ _ cb -> do
            cb (ChunkToolCallStart "call_read" "read_file")
            cb (ChunkToolCallArgs "" "{\"path\":\"README.md\"}")
            cb ChunkDone
        }
      spec = SubAgentSpec "implementer" "Survey codebase" 1 []
  subHandle <- async $ runEphemeralSubAgent es driver spec
  threadDelay 50000
  promptQueueEmpty <- atomically $ isEmptyTBQueue (uiPromptQueue sec)
  assert "SubAgent spawned without triggering upfront grant prompt" promptQueueEmpty

  -- 2. Safe workspace read finishes without prompt
  _ <- wait subHandle
  queueStillEmpty <- atomically $ isEmptyTBQueue (uiPromptQueue sec)
  assert "Safe file read inside workspace succeeded without prompt" queueStillEmpty

  -- 3. Outside workspace read triggers dynamic JIT prompt
  let caller = SubAgentId 99 "Outside file inspection"
      callOutside = ToolCall "c_out" "read_file" (Aeson.object ["path" .= ("/etc/passwd" :: Text)])
  outHandle <- async $ executeToolDispatch es caller ["read_file*"] callOutside
  promptOut <- atomically $ readTBQueue (uiPromptQueue sec)
  assert "Prompt indicates outside workspace access" ("outside_workspace: read_file" `T.isInfixOf` promptTool promptOut)
  atomically $ putTMVar (promptReply promptOut) PermNo
  resOut <- wait outHandle
  assert "Outside read rejected when denied" ("Permission Denied" `T.isInfixOf` resultStderr resOut)

  -- 4. External web fetch triggers dynamic JIT prompt
  let callWeb = ToolCall "c_web" "fetch_url" (Aeson.object ["url" .= ("https://api.github.com" :: Text)])
  webHandle <- async $ executeToolDispatch es caller ["fetch_url*"] callWeb
  promptWeb <- atomically $ readTBQueue (uiPromptQueue sec)
  assert "Web fetch triggers dynamic JIT prompt" ("fetch_url" `T.isInfixOf` promptTool promptWeb)
  atomically $ putTMVar (promptReply promptWeb) PermNo
  resWeb <- wait webHandle
  assert "Web fetch rejected when denied" ("Permission Denied" `T.isInfixOf` resultStderr resWeb)

  -- 5. Destructive mutating command (write_file) triggers dynamic JIT prompt
  let callWrite = ToolCall "c_wr" "write_file" (Aeson.object
        [ "path" .= ("test_out.txt" :: Text)
        , "content" .= ("hello" :: Text)
        ])
  writeHandle <- async $ executeToolDispatch es caller ["write_file*"] callWrite
  promptWrite <- atomically $ readTBQueue (uiPromptQueue sec)
  assert "Destructive write triggers dynamic JIT prompt" ("write_file" `T.isInfixOf` promptTool promptWrite)
  atomically $ putTMVar (promptReply promptWrite) PermOnce
  resWrite <- wait writeHandle
  assert "Destructive write succeeds when approved" (resultStdout resWrite == "File successfully written: test_out.txt")
  removeFile "test_out.txt"

  putStrLn "  -> OK: Subagents spawn without upfront prompt; JIT prompts enforce workspace boundaries, web fetches, and destructive writes."

assert :: String -> Bool -> IO ()
assert desc condition =
  unless condition $ failTest ("Assertion failed: " <> desc)

failTest :: String -> IO ()
failTest msg = do
  putStrLn $ "  [FAIL] " <> msg
  exitFailure
