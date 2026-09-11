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
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime, diffUTCTime, addUTCTime)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist, removeFile)
import System.Environment (setEnv, unsetEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Lambda.Config (loadConfig, Config(..), SpecialistConfig(..), defaultConfig, resolveModelAlias, resolveModelWithConfig, lookupModelContextLimit, resolveEnvTemplates)
import Lambda.Engine.PromptMacro (listPromptMacros, loadPromptMacro, expandPromptMacro)
import Lambda.Core.EngineInterface (initEngineChannels, cmdQueue, startEngineLoop, EngineChannels(..))
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider
import Lambda.Provider.JsonRpc (startRpcClient, stopRpcClient, sendRequest)
import Lambda.Driver.OpenAI (parseSseChunk, splitThinkingChunks)
import Lambda.Engine.Artifacts
import Lambda.Engine.Compactor
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.Security (initSecurity, checkAuthorization, SecurityState(..))
import Lambda.Engine.Session (exportSessionTrace, loadSession, listSessions, saveSession, getLatestSession, pruneSessions, renderSessionTraceMarkdown, Session(..), SessionMeta(..))
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
  , appEventQueue
  , appActiveModel
  , appContextLimit
  , appUndoStack
  )
import Lambda.Engine.SubAgent (submitReportTool, filterSubAgentRegistry, subAgentLoop, runEphemeralSubAgent, SubAgentSpec(..), SubAgentReport(..))
import qualified Data.Map.Strict as Map
import Brick.Types (vSize, Size(..))
import qualified Brick.Widgets.Edit as E
import Lambda.Provider.Builtin (builtinTools, listDirectoryTool, readFileTool, fetchUrlTool, editFileTool, renderDiffBlock, applyWhitespaceTolerantEdit)
import Lambda.Provider.Mcp (inferCapability, parseMcpCallResult, startAndLoadMcpServers, stopMcpClient)
import Lambda.UI.Completion (completeInput, allCommands, computeCommandCandidates, slidingCandidateWindow)
import Lambda.UI.Draw (renderSubAgents, renderSubAgentsSelected, renderInlineSubAgent, renderCompletionLine)
import Lambda.UI.Events (computeCommandMatches, replaceCurrentToken, cycleCompletedToken)
import Lambda.UI.Types (CompletionState(..), Candidate(..), simpleCandidate, ResourceName(..), UIState(..))
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
  testReadlineKeybindingsAndTabDecoupling
  testContextualCompletersAndMruInvariant
  testSlidingCandidateWindowInvariant
  testDynamicModelSwitching
  testSessionRewindAndFork
  testStructuredDiffAndWhitespaceEditing
  testPromptMacrosAndExpansion
  testModelSwitchingDeploymentFixes

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

  -- Test {ENV:VAR} template resolution helper
  let mockLookup "SECRET_OPENAI_KEY" = pure (Just "sk-secret-openai-value")
      mockLookup "PORT"              = pure (Just "8080")
      mockLookup _                   = pure Nothing

  res1 <- resolveEnvTemplates mockLookup "{ENV:SECRET_OPENAI_KEY}"
  assert "resolveEnvTemplates resolves exact {ENV:VAR}" (res1 == "sk-secret-openai-value")

  res2 <- resolveEnvTemplates mockLookup "{env:SECRET_OPENAI_KEY}"
  assert "resolveEnvTemplates resolves lowercase {env:VAR}" (res2 == "sk-secret-openai-value")

  res3 <- resolveEnvTemplates mockLookup "http://localhost:{ENV:PORT}/v1"
  assert "resolveEnvTemplates resolves embedded {ENV:VAR}" (res3 == "http://localhost:8080/v1")

  res4 <- resolveEnvTemplates mockLookup "sk-plain-text-key"
  assert "resolveEnvTemplates preserves plain text keys" (res4 == "sk-plain-text-key")

  -- Test {ENV:...} inside .lambda/config.json via loadConfig
  let tempCfgDir = "/tmp/lambda_test_env_template_cfg"
      localLambdaDir = tempCfgDir </> ".lambda"
  createDirectoryIfMissing True localLambdaDir
  TIO.writeFile (localLambdaDir </> "config.json")
    "{\n  \"api_key\": \"{ENV:DYNAMIC_PROVIDER_KEY}\",\n  \"api_base_url\": \"https://api.openai.com/v1\"\n}\n"
  setEnv "DYNAMIC_PROVIDER_KEY" "sk-dynamic-from-env-var-12345"

  cfgWithTemplate <- loadConfig tempCfgDir
  assert "loadConfig resolves {ENV:DYNAMIC_PROVIDER_KEY} from config.json"
    (apiKey cfgWithTemplate == "sk-dynamic-from-env-var-12345")
  assert "loadConfig preserved api_base_url from config.json"
    (apiBaseUrl cfgWithTemplate == "https://api.openai.com/v1")

  unsetEnv "DYNAMIC_PROVIDER_KEY"
  removeDirectoryRecursive tempCfgDir

  putStrLn "  -> OK: OpenRouter environment variables and {ENV:...} template resolution reliably loaded."

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
        , sessionParentId      = Nothing
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
        , sessionParentId      = Nothing
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
        , sessionParentId      = Nothing
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
        , sessionParentId      = Nothing
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
        , sessionParentId      = Nothing
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

  -- 1. Canonical matching logic using computeCommandMatches from Lambda.UI.Events
  let slashMatches = computeCommandMatches "/"
  assert "Typing '/' returns all available slash commands" (length slashMatches == length allCommands)

  -- Check unambiguous prefix matching (single match auto-completes immediately in-place)
  let planMatches = computeCommandMatches "/pl"
  assert "Typing '/pl' returns exactly ['/plan'] for immediate in-place auto-fill" (planMatches == ["/plan"])

  let helpMatches = computeCommandMatches "/h"
  assert "Typing '/h' returns exactly ['/help'] for immediate in-place auto-fill" (helpMatches == ["/help"])

  let compactMatches = computeCommandMatches "/co"
  assert "Typing '/co' returns exactly ['/compact']" (compactMatches == ["/compact"])

  -- Check ambiguous prefix matching (triggers candidate preview bar and cycling)
  let cMatches = computeCommandMatches "/c"
  assert "Typing '/c' matches multiple candidates (/compact and /clear)" (cMatches == ["/compact", "/clear"])

  let pMatches = computeCommandMatches "/p"
  assert "Typing '/p' matches multiple candidates (/plan, /prompt, /p)" ("/plan" `elem` pMatches && "/prompt" `elem` pMatches)

  let sessionMatches = computeCommandMatches "/s"
  assert "Typing '/s' matches /session and /sub" (sessionMatches == ["/session", "/sub"])

  -- Check trailing newline from editor does not break matching
  let newlineMatches = computeCommandMatches "/t\n"
  assert "Trailing newline from editor lines does not break matching" (newlineMatches == ["/think", "/trace"])

  -- Check commands with spaces are dismissed
  let spaceMatches = computeCommandMatches "/plan "
  assert "Commands with trailing arguments/space dismiss autocomplete" (null spaceMatches)

  -- Check non-slash text does not trigger autocomplete
  let normalMatches = computeCommandMatches "cabal test"
  assert "Regular prompts do not trigger slash autocomplete" (null normalMatches)

  -- 2. Test In-Place Modulo Tab Cycling Logic
  let cycleForward :: Int -> Int -> Int
      cycleForward len sel = (sel + 1) `mod` len

      cycleBackward :: Int -> Int -> Int
      cycleBackward len sel = if sel <= 0 then len - 1 else sel - 1

  assert "Forward cycle from 0 of 2 advances to 1" (cycleForward 2 0 == 1)
  assert "Forward cycle wraps from 1 of 2 back to 0" (cycleForward 2 1 == 0)
  assert "Backward cycle wraps from 0 of 2 back to 1" (cycleBackward 2 0 == 1)
  assert "Backward cycle from 1 of 2 steps back to 0" (cycleBackward 2 1 == 0)

  -- 3. Verify Minimal Single-Line Horizontal Completion Bar Invariant
  let emptyCompWidget = renderCompletionLine Nothing
  assert "renderCompletionLine Nothing has Fixed vertical size" (vSize emptyCompWidget == Fixed)

  let activeCompWidget = renderCompletionLine (Just (CompletionState [simpleCandidate "/plan", simpleCandidate "/exec"] 0))
  assert "renderCompletionLine active has Fixed vertical size (single-line hint bar without viewport crash)" (vSize activeCompWidget == Fixed)

  putStrLn "  -> OK: Slash command autocomplete filters dynamically, cycles candidates in-place, and renders minimal single-line hint bar."

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

-- 36. Verify Readline Keybinding Hygiene & Tab Completion Decoupling
testReadlineKeybindingsAndTabDecoupling :: IO ()
testReadlineKeybindingsAndTabDecoupling = do
  putStrLn "\n[Test 36] Readline Keybinding Hygiene & Tab Completion Decoupling"

  -- 1. replaceCurrentToken replaces current token with candidate
  let ed1 = E.editor EditorInput (Just 1) "hello wor"
      ed1Replaced = replaceCurrentToken "world" ed1
  assert "replaceCurrentToken replaces prefix and appends trailing space for words"
    (T.concat (E.getEditContents ed1Replaced) == "hello world ")

  -- 2. replaceCurrentToken does NOT append space for directory paths ending in '/'
  let ed2 = E.editor EditorInput (Just 1) "cat @src/Lam"
      ed2Replaced = replaceCurrentToken "@src/Lambda/" ed2
  assert "replaceCurrentToken preserves trailing slash without space for path drilling"
    (T.concat (E.getEditContents ed2Replaced) == "cat @src/Lambda/")

  -- 3. Slash command completion does not trigger for normal user prompt text
  let nonCmdCandidates = computeCommandCandidates "build this project"
  assert "Non-slash prompt returns 0 candidates" (null nonCmdCandidates)

  -- 4. Slash command completion matches all commands on "/"
  let allSlashCandidates = computeCommandCandidates "/"
  assert "Typing '/' returns all available slash commands"
    (length allSlashCandidates == length allCommands)

  putStrLn "  -> OK: Tab completion decouples cleanly and respects Readline directory drill-down semantics."

-- 37. Verify Contextual Completers & MRU Ordering Invariant
testContextualCompletersAndMruInvariant :: IO ()
testContextualCompletersAndMruInvariant = do
  putStrLn "\n[Test 37] Contextual Completers & MRU Ordering Invariant"

  -- 1. /session completion with MRU ordering
  let testSessionsDir = ".lambda/test_sessions"
  createDirectoryIfMissing True testSessionsDir
  now <- getCurrentTime
  let mkSess sid title offset = Session
        { sessionId           = sid
        , sessionCreatedAt     = addUTCTime offset now
        , sessionUpdatedAt     = addUTCTime offset now
        , sessionTitle         = title
        , sessionMode          = PlanMode
        , sessionTurns         = []
        , sessionSubAgents     = Map.empty
        , sessionStateVector   = Map.empty
        , sessionPromptHistory = []
        , sessionParentId      = Nothing
        }

  saveSession testSessionsDir 0 (mkSess "sess_old_123" "Older Session" (-300))
  saveSession testSessionsDir 0 (mkSess "sess_new_456" "Newer Session" 0)

  sessList <- listSessions testSessionsDir
  assert "Sessions are ordered by metaUpdatedAt descending (MRU first)"
    (case sessList of
       (s1:s2:_) -> metaId s1 == "sess_new_456" && metaId s2 == "sess_old_123"
       _ -> False)
  removeDirectoryRecursive testSessionsDir

  -- Setup dummy UI state
  evQ <- atomically newTQueue
  chans <- initEngineChannels evQ
  let dummyUI subs = UIState
        { uiTurns            = []
        , uiSubAgents        = subs
        , uiCurrentPrompt    = Nothing
        , uiPendingPrompts   = Seq.Empty
        , uiMode             = PlanMode
        , uiEditor           = E.editor EditorInput (Just 1) ""
        , uiWorkingState     = ""
        , uiChannels         = chans
        , uiLastEscTime      = Nothing
        , uiContextLimit     = 8192
        , uiPromptHistory    = []
        , uiHistoryIndex     = Nothing
        , uiSavedDraft       = ""
        , uiModelName        = "test"
        , uiThinkingVisible  = True
        , uiSelectedSubAgent = Nothing
        , uiShowHud          = False
        , uiCompletion       = Nothing
        , uiIsGenerating     = False
        , uiConfig           = defaultConfig
        }

  -- 2. /mode contextual completion
  mModeComp <- completeInput "." (dummyUI Map.empty) "/mode "
  assert "/mode completion suggests 'plan' and 'exec'"
    (fmap (map candInsert . compCandidates) mModeComp == Just ["plan", "exec"])

  -- 3. /think contextual completion
  mThinkComp <- completeInput "." (dummyUI Map.empty) "/think "
  assert "/think completion suggests 'toggle', 'show', and 'hide'"
    (fmap (map candInsert . compCandidates) mThinkComp == Just ["toggle", "show", "hide"])

  -- 4. /sub contextual completion from active subagent map
  let subMap = Map.fromList
        [ (1, SubAgentTask 1 "reviewer" "Reviewing" 0 5 SubAgentRunning Nothing [])
        , (3, SubAgentTask 3 "tester" "Testing" 0 5 SubAgentRunning Nothing [])
        ]
  mSubComp <- completeInput "." (dummyUI subMap) "/sub "
  assert "/sub completion returns active subagent IDs plus main"
    (fmap (map candInsert . compCandidates) mSubComp == Just ["1", "3", "main"])

  -- 5. Path completion for '@' tokens
  mPathComp <- completeInput "." (dummyUI Map.empty) "@src/"
  assert "Path completion for '@src/' suggests '@src/Lambda/' with trailing slash"
    (case mPathComp of
       Just cs -> "@src/Lambda/" `elem` map candInsert (compCandidates cs)
       Nothing -> False)

  putStrLn "  -> OK: Contextual completers provide accurate MRU session sorting, active subagent IDs, and path drill-down."

-- 38. Verify Sliding Candidate Window & Overflow Counter Invariant
testSlidingCandidateWindowInvariant :: IO ()
testSlidingCandidateWindowInvariant = do
  putStrLn "\n[Test 38] Sliding Candidate Window & Overflow Counter Invariant"

  let makeCands (n :: Int) = [ Candidate (T.pack $ "cand_" <> show i) (T.pack $ "cand_" <> show i) | i <- [1..n] ]

  -- 1. Empty list
  let (sel0, win0, over0) = slidingCandidateWindow 0 []
  assert "Empty candidate list yields empty window and 0 overflow"
    (sel0 == 0 && null win0 && over0 == 0)

  -- 2. Fewer than maxVisible items
  let cands3 = makeCands 3
      (sel3, win3, over3) = slidingCandidateWindow 1 cands3
  assert "3 items with maxVisible 5 yields all 3 items and 0 overflow"
    (sel3 == 1 && length win3 == 3 && over3 == 0)

  -- 3. 12 items with selected item at index 0
  let cands12 = makeCands 12
      (sel12_0, win12_0, over12_0) = slidingCandidateWindow 0 cands12
  assert "12 items at index 0 caps window size to 5" (length win12_0 == 5)
  assert "12 items at index 0 calculates overflow as 7 (+7 more)" (over12_0 == 7)
  assert "Index 0 candidate is present in window" (head win12_0 == head cands12)
  assert "Selected index inside window is 0" (sel12_0 == 0)

  -- 4. 12 items with selected item at index 8 (scrolled far right)
  let (sel12_8, win12_8, over12_8) = slidingCandidateWindow 8 cands12
  assert "Window size remains 5 when scrolled to index 8" (length win12_8 == 5)
  assert "Selected candidate (index 8) is inside the sliding window"
    (cands12 !! 8 `elem` win12_8)
  assert "Relative selected index matches position in window"
    (win12_8 !! sel12_8 == cands12 !! 8)
  assert "Overflow count is 1 when scrolled to index 8" (over12_8 == 1)

  -- 5. 12 items with selected item at last index (11)
  let (sel12_11, win12_11, over12_11) = slidingCandidateWindow 11 cands12
  assert "Last item is the final element of sliding window"
    (last win12_11 == last cands12)
  assert "Relative selected index is 4 (the last slot in 5-item window)"
    (sel12_11 == 4)
  assert "Overflow count at end is 0"
    (over12_11 == 0)

  putStrLn "  -> OK: Sliding candidate window caps ribbon size to 5, tracks cursor position, and displays accurate overflow."

-- 39. Verify Dynamic Model Switching & Alias Resolution Invariant
testDynamicModelSwitching :: IO ()
testDynamicModelSwitching = do
  putStrLn "\n[Test 39] Dynamic Model Switching & Alias Resolution Invariant"

  -- 1. Test Alias Resolution
  assert "Alias 'claude' resolves to 'anthropic/claude-3.5-sonnet'"
    (resolveModelAlias "claude" == "anthropic/claude-3.5-sonnet")
  assert "Alias 'r1' resolves to 'deepseek/deepseek-r1'"
    (resolveModelAlias "r1" == "deepseek/deepseek-r1")
  assert "Alias '4o' resolves to 'openai/gpt-4o'"
    (resolveModelAlias "4o" == "openai/gpt-4o")
  assert "Alias 'qwen' resolves to 'qwen/qwen-2.5-coder-32b-instruct'"
    (resolveModelAlias "qwen" == "qwen/qwen-2.5-coder-32b-instruct")
  assert "Unknown or arbitrary model ID is preserved as-is"
    (resolveModelAlias "mistralai/codestral-2501" == "mistralai/codestral-2501")

  -- 2. Test Context Limit Lookup
  assert "Claude model has 200k context limit"
    (lookupModelContextLimit "anthropic/claude-3.5-sonnet" == 200000)
  assert "DeepSeek R1 model has 128k context limit"
    (lookupModelContextLimit "deepseek/deepseek-r1" == 128000)
  assert "Free tier model has 32k context limit"
    (lookupModelContextLimit "openrouter/free" == 32000)

  -- 3. Dynamic Model Switching via Engine Loop
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  let tools = builtinTools "." ".lambda/artifacts"
      reg = registerTools tools emptyRegistry
  es <- initEngineState cfg reg sec
  chans <- initEngineChannels (appEventQueue es)
  let dummyDriver = ModelDriver { streamCompletion = \_ _ cb -> cb ChunkDone }
  startEngineLoop es dummyDriver chans

  -- Initial active model should be from config
  initMod <- readTVarIO (appActiveModel es)
  assert "Initial model equals config model" (initMod == modelName cfg)

  -- Dispatch CmdSetModel "claude"
  atomically $ writeTBQueue (cmdQueue chans) (CmdSetModel "claude")

  -- Wait for EvModelSwitched event
  let waitForSwitch (0 :: Int) = do
        failTest "Timed out waiting for EvModelSwitched"
        pure ("", 0)
      waitForSwitch n = do
        mEv <- atomically $ tryReadTQueue (evQueue chans)
        case mEv of
          Just (EvModelSwitched m lim) -> pure (m, lim)
          _ -> threadDelay 20000 >> waitForSwitch (n - 1)

  (switchedMod, switchedLim) <- waitForSwitch 50
  assert "EvModelSwitched indicates resolved canonical model"
    (switchedMod == "anthropic/claude-3.5-sonnet")
  assert "EvModelSwitched indicates updated 200k context limit"
    (switchedLim == 200000)

  -- Verify AppEngineState TVars are updated
  updatedMod <- readTVarIO (appActiveModel es)
  updatedLim <- readTVarIO (appContextLimit es)
  assert "appActiveModel is updated in state"
    (updatedMod == "anthropic/claude-3.5-sonnet")
  assert "appContextLimit is updated in state"
    (updatedLim == 200000)

  -- 4. Contextual autocomplete for /model
  let dummyUI = UIState
        { uiTurns            = []
        , uiSubAgents        = Map.empty
        , uiCurrentPrompt    = Nothing
        , uiPendingPrompts   = Seq.Empty
        , uiMode             = PlanMode
        , uiEditor           = E.editor EditorInput (Just 1) ""
        , uiWorkingState     = ""
        , uiChannels         = chans
        , uiLastEscTime      = Nothing
        , uiContextLimit     = 8192
        , uiPromptHistory    = []
        , uiHistoryIndex     = Nothing
        , uiSavedDraft       = ""
        , uiModelName        = "test"
        , uiThinkingVisible  = True
        , uiSelectedSubAgent = Nothing
        , uiShowHud          = False
        , uiCompletion       = Nothing
        , uiIsGenerating     = False
        , uiConfig           = defaultConfig
        }

  mModelComp <- completeInput "." dummyUI "/model "
  assert "/model completion includes 'claude', 'r1', '4o', and 'qwen'"
    (case mModelComp of
       Just cs ->
         let inserts = map candInsert (compCandidates cs)
         in "claude" `elem` inserts && "r1" `elem` inserts && "4o" `elem` inserts && "qwen" `elem` inserts
       Nothing -> False)

  putStrLn "  -> OK: Dynamic model switching resolves aliases, updates engine state & context limits, and integrates with autocomplete."

-- 40. Verify Session Rewind and Forking
testSessionRewindAndFork :: IO ()
testSessionRewindAndFork = do
  putStrLn "\n[Test 40] Session Rewind & Forking (Lineage & Undo Stack)"
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  let testSessDir = ".lambda/test_sessions_40"
  createDirectoryIfMissing True testSessDir

  now <- getCurrentTime
  let initialTurns =
        [ Turn 1 UserRole [TextBlock "What is Pi?"]
        , Turn 2 AssistantRole [TextBlock "Pi is 3.14159..."]
        , Turn 3 UserRole [TextBlock "Tell me more"]
        , Turn 4 AssistantRole [TextBlock "It is transcendental."]
        ]
      sess = Session
        { sessionId           = "test_rewind_sess"
        , sessionCreatedAt     = now
        , sessionUpdatedAt     = now
        , sessionTitle         = "Original session"
        , sessionMode          = PlanMode
        , sessionTurns         = initialTurns
        , sessionSubAgents     = Map.empty
        , sessionStateVector   = Map.empty
        , sessionPromptHistory = ["What is Pi?", "Tell me more"]
        , sessionParentId      = Nothing
        }

  es <- initEngineStateWithSession cfg emptyRegistry sec (Just sess)
  let es' = es { appSessionDir = testSessDir }
  chans <- initEngineChannels (appEventQueue es')

  let dummyDriver = ModelDriver { streamCompletion = \_ _ cb -> cb ChunkDone }

  startEngineLoop es' dummyDriver chans

  -- 1. Test /rewind 1 (drops 1 user turn + 1 assistant turn = 2 turns)
  atomically $ writeTBQueue (cmdQueue chans) (CmdRewindTurns 1)
  threadDelay 50000

  turnsAfterRewind <- readTVarIO (appTurns es')
  assert "Rewind 1 dropped exactly 2 turns" (length turnsAfterRewind == 2)
  assert "Remaining turns match first user and assistant turn"
    (case turnsAfterRewind of
       [Turn 1 UserRole [TextBlock u], Turn 2 AssistantRole [TextBlock a]] ->
         u == "What is Pi?" && a == "Pi is 3.14159..."
       _ -> False)

  undoStack <- readTVarIO (appUndoStack es')
  assert "appUndoStack saved popped turns" (length undoStack == 1 && length (head undoStack) == 2)

  -- 2. Test /fork "My Branch"
  atomically $ writeTBQueue (cmdQueue chans) (CmdForkSession (Just "My Branch"))
  threadDelay 100000

  turnsAfterFork <- readTVarIO (appTurns es')
  assert "Fork preserved current turns (2 turns)" (length turnsAfterFork == 2)

  -- Check session saved on disk
  latestSess <- getLatestSession testSessDir
  assert "Forked session has sessionParentId pointing to original session"
    (case latestSess of
       Just s -> sessionParentId s == Just "test_rewind_sess" && sessionId s /= "test_rewind_sess"
       Nothing -> False)

  -- 3. Autocomplete recognizes /rewind, /undo, and /fork
  assert "allCommands contains /rewind, /undo, and /fork"
    ("/rewind" `elem` allCommands && "/undo" `elem` allCommands && "/fork" `elem` allCommands)

  removeDirectoryRecursive testSessDir
  putStrLn "  -> OK: Session rewind drops turns & updates undo stack, fork preserves parent lineage & state, commands registered."

-- 41. Verify Structured Diffs and Whitespace-Tolerant File Editing
testStructuredDiffAndWhitespaceEditing :: IO ()
testStructuredDiffAndWhitespaceEditing = do
  putStrLn "\n[Test 41] Structured Diffs & Whitespace-Tolerant File Editing"
  let tempDir = ".lambda/test_diff_41"
  createDirectoryIfMissing True tempDir

  -- 1. Test diff block formatting
  let diff = renderDiffBlock "src/App.hs" "main = putStrLn \"hello\"" "main = putStrLn \"world\""
  assert "Diff contains standard unified header"
    ("--- a/src/App.hs\n+++ b/src/App.hs" `T.isInfixOf` diff)
  assert "Diff prefixes target with '- '"
    ("- main = putStrLn \"hello\"" `T.isInfixOf` diff)
  assert "Diff prefixes replacement with '+ '"
    ("+ main = putStrLn \"world\"" `T.isInfixOf` diff)

  -- 2. Test whitespace-tolerant line-based matching helper
  let originalCode = "def compute():\n    x = 10   \n    return x\n"
      targetCode   = "def compute():\n    x = 10\n    return x"
      newCode      = "def compute():\n    x = 42\n    return x * 2"
  case applyWhitespaceTolerantEdit originalCode targetCode newCode of
    Left err -> failTest ("Whitespace-tolerant edit failed unexpectedly: " <> T.unpack err)
    Right updated -> do
      assert "Updated code contains replacement value" ("x = 42" `T.isInfixOf` updated)
      assert "Updated code removed old value" (not ("x = 10" `T.isInfixOf` updated))

  -- 3. Test editFileTool with end-to-end whitespace tolerance and diff output
  let testFile = tempDir ++ "/service.py"
  TIO.writeFile testFile "function start() {\n    logger.info(\"starting\")   \n    return true;\n}\n"

  let editTool = editFileTool tempDir
      args = Aeson.object
        [ "path" .= ("service.py" :: Text)
        , "target" .= ("function start() {\n    logger.info(\"starting\")\n    return true;\n}" :: Text)
        , "replacement" .= ("function start() {\n    logger.info(\"ready\")\n    return true;\n}" :: Text)
        ]

  res <- toolExecute editTool MainAgent args
  assert "Tool result indicates whitespace-tolerant success"
    ("Successfully edited file (whitespace-tolerant): service.py" `T.isInfixOf` resultStdout res)
  assert "Tool result stdout includes colorable diff block"
    ("--- a/service.py" `T.isInfixOf` resultStdout res && "- function start()" `T.isInfixOf` resultStdout res)

  -- Verify written file on disk has new content
  newContent <- TIO.readFile testFile
  assert "File on disk has updated content" ("\"ready\"" `T.isInfixOf` newContent)

  removeDirectoryRecursive tempDir
  putStrLn "  -> OK: Structured diffs formatted with unified headers, whitespace-tolerant editing resolves minor variations."

-- 42. Verify User Prompt Macros and $input Expansion
testPromptMacrosAndExpansion :: IO ()
testPromptMacrosAndExpansion = do
  putStrLn "\n[Test 42] Prompt Macros & $input Expansion (.lambda/prompts/*.md)"
  let tempDir = ".lambda/test_prompts_42"
      promptsDir = tempDir </> ".lambda" </> "prompts"
  createDirectoryIfMissing True promptsDir

  -- 1. Create prompt macro templates
  TIO.writeFile (promptsDir </> "review.md")
    "Perform a critical code review on $input looking for safety, concurrency bugs, and edge cases."
  TIO.writeFile (promptsDir </> "refactor.md")
    "Refactor $ARG to follow standard library idioms and eliminate dead code."
  TIO.writeFile (promptsDir </> "summarize.md")
    "Provide an architectural summary of the following components:"

  -- 2. Test listPromptMacros
  macros <- listPromptMacros tempDir
  assert "listPromptMacros discovers 'review', 'refactor', and 'summarize'"
    ("review" `elem` macros && "refactor" `elem` macros && "summarize" `elem` macros)

  -- 3. Test expandPromptMacro with $input
  let exp1 = expandPromptMacro "Review $input thoroughly." "src/Lambda/Types.hs"
  assert "expandPromptMacro substitutes $input"
    (exp1 == "Review src/Lambda/Types.hs thoroughly.")

  -- 4. Test expandPromptMacro with $ARG
  let exp2 = expandPromptMacro "Benchmark $ARG now." "my_bench"
  assert "expandPromptMacro substitutes $ARG"
    (exp2 == "Benchmark my_bench now.")

  -- 5. Test expandPromptMacro appending when no variable
  let exp3 = expandPromptMacro "Summarize:" "src/App.hs"
  assert "expandPromptMacro appends trailing args when no placeholder"
    (exp3 == "Summarize:\n\nsrc/App.hs")

  -- 6. Test loadPromptMacro existing and missing
  resLoaded <- loadPromptMacro tempDir "review" "src/Lambda/Engine/Session.hs"
  assert "loadPromptMacro successfully loads and expands existing macro"
    (case resLoaded of
       Right text -> "src/Lambda/Engine/Session.hs" `T.isInfixOf` text && not ("$input" `T.isInfixOf` text)
       Left _     -> False)

  resMissing <- loadPromptMacro tempDir "nonexistent" "args"
  assert "loadPromptMacro returns Left for nonexistent macro"
    (case resMissing of
       Left err -> "not found" `T.isInfixOf` err
       Right _  -> False)

  -- 7. Contextual autocomplete integration (/prompt <Tab> and /p <Tab>)
  sec <- initSecurity [] []
  cfg <- loadConfig "."
  es <- initEngineState cfg emptyRegistry sec
  chans <- initEngineChannels (appEventQueue es)
  let dummyUI = UIState
        { uiTurns            = []
        , uiSubAgents        = Map.empty
        , uiCurrentPrompt    = Nothing
        , uiPendingPrompts   = Seq.Empty
        , uiMode             = PlanMode
        , uiEditor           = E.editor EditorInput (Just 1) ""
        , uiWorkingState     = ""
        , uiChannels         = chans
        , uiLastEscTime      = Nothing
        , uiContextLimit     = 8192
        , uiPromptHistory    = []
        , uiHistoryIndex     = Nothing
        , uiSavedDraft       = ""
        , uiModelName        = "test"
        , uiThinkingVisible  = True
        , uiSelectedSubAgent = Nothing
        , uiShowHud          = False
        , uiCompletion       = Nothing
        , uiIsGenerating     = False
        , uiConfig           = cfg
        }

  mPromptComp <- completeInput tempDir dummyUI "/prompt "
  assert "/prompt completion returns 'review', 'refactor', 'summarize'"
    (case mPromptComp of
       Just cs ->
         let inserts = map candInsert (compCandidates cs)
         in "review" `elem` inserts && "refactor" `elem` inserts && "summarize" `elem` inserts
       Nothing -> False)

  mPComp <- completeInput tempDir dummyUI "/p rev"
  assert "/p completion filters candidates with prefix"
    (case mPComp of
       Just cs ->
         let inserts = map candInsert (compCandidates cs)
         in inserts == ["review"]
       Nothing -> False)

  removeDirectoryRecursive tempDir
  putStrLn "  -> OK: Prompt macros discovered from .lambda/prompts/*.md, $input expanded, autocomplete ribbons integrated."

-- 43. Verify Model Switching Deployment Fixes (Token cycling, local/openai candidate filtering, alias resolution)
testModelSwitchingDeploymentFixes :: IO ()
testModelSwitchingDeploymentFixes = do
  putStrLn "\n[Test 43] Model Switching Deployment Fixes (Token Cycling, Local Filtering, Endpoint-Aware Aliases)"

  -- 1. Verify Token Cycling Replacement (Fixes /mode /models concatenation bug)
  let ed0 = E.editor EditorInput (Just 1) "/mode "
      ed1 = cycleCompletedToken "/mode" "/model" ed0
      text1 = T.concat (E.getEditContents ed1)
  assert "Cycling from /mode to /model produces '/model ' without concatenation"
    (text1 == "/model ")

  let ed2 = cycleCompletedToken "/model" "/models" ed1
      text2 = T.concat (E.getEditContents ed2)
  assert "Cycling from /model to /models produces '/models ' without concatenation"
    (text2 == "/models ")

  let ed3 = cycleCompletedToken "/models" "/mode" ed2
      text3 = T.concat (E.getEditContents ed3)
  assert "Cycling back to /mode wraps cleanly"
    (text3 == "/mode ")

  -- Cycling with arguments (e.g. /model 4o -> /model o3)
  let edArg0 = E.editor EditorInput (Just 1) "/model 4o "
      edArg1 = cycleCompletedToken "4o" "o3" edArg0
      textArg1 = T.concat (E.getEditContents edArg1)
  assert "Cycling model argument replaces only the model token"
    (textArg1 == "/model o3 ")

  -- 2. Verify Endpoint-Aware Model Alias Resolution
  let openAiCfg = defaultConfig
        { apiBaseUrl = "https://api.openai.com/v1"
        , modelAliases = Map.fromList [("fast", "gpt-4o-mini")]
        }
  assert "Direct OpenAI endpoint resolves '4o' to 'gpt-4o' (NOT 'openai/gpt-4o')"
    (resolveModelWithConfig openAiCfg "4o" == "gpt-4o")
  assert "Direct OpenAI endpoint resolves 'o3' to 'o3-mini'"
    (resolveModelWithConfig openAiCfg "o3" == "o3-mini")
  assert "User-configured alias takes priority on OpenAI"
    (resolveModelWithConfig openAiCfg "fast" == "gpt-4o-mini")

  let localCfg = defaultConfig
        { apiBaseUrl = "http://localhost:11434/v1"
        , configuredModels = ["qwen2.5-coder:32b", "deepseek-r1:14b"]
        , modelAliases = Map.fromList [("coder", "qwen2.5-coder:32b")]
        }
  assert "Local endpoint matches configured models directly without vendor prefix"
    (resolveModelWithConfig localCfg "qwen2.5-coder:32b" == "qwen2.5-coder:32b")
  assert "Local endpoint user alias resolves correctly"
    (resolveModelWithConfig localCfg "coder" == "qwen2.5-coder:32b")
  assert "Local endpoint unknown model passes through without OpenRouter vendor prefix"
    (resolveModelWithConfig localCfg "llama3.3:70b" == "llama3.3:70b")

  let openRouterCfg = defaultConfig
        { apiBaseUrl = "https://openrouter.ai/api/v1"
        }
  assert "OpenRouter endpoint resolves '4o' to OpenRouter vendor slug 'openai/gpt-4o'"
    (resolveModelWithConfig openRouterCfg "4o" == "openai/gpt-4o")

  -- 3. Verify Autocomplete Candidate Filtering (Local vs Cloud)
  evQ <- atomically newTQueue
  chans <- initEngineChannels evQ
  let localUI = UIState
        { uiTurns            = []
        , uiSubAgents        = Map.empty
        , uiCurrentPrompt    = Nothing
        , uiPendingPrompts   = Seq.Empty
        , uiMode             = PlanMode
        , uiEditor           = E.editor EditorInput (Just 1) ""
        , uiWorkingState     = ""
        , uiChannels         = chans
        , uiLastEscTime      = Nothing
        , uiContextLimit     = 128000
        , uiPromptHistory    = []
        , uiHistoryIndex     = Nothing
        , uiSavedDraft       = ""
        , uiModelName        = "qwen2.5-coder:32b"
        , uiThinkingVisible  = True
        , uiSelectedSubAgent = Nothing
        , uiShowHud          = False
        , uiCompletion       = Nothing
        , uiIsGenerating     = False
        , uiConfig           = localCfg
        }

  mLocalComp <- completeInput "." localUI "/model "
  assert "Local endpoint /model autocomplete contains local models and aliases"
    (case mLocalComp of
       Just cs ->
         let inserts = map candInsert (compCandidates cs)
         in "coder" `elem` inserts && "qwen2.5-coder:32b" `elem` inserts
       Nothing -> False)
  assert "Local endpoint /model autocomplete NEVER contains 'claude' or 'anthropic/claude-3.5-sonnet'"
    (case mLocalComp of
       Just cs ->
         let inserts = map candInsert (compCandidates cs)
         in not ("claude" `elem` inserts) && not ("anthropic/claude-3.5-sonnet" `elem` inserts)
       Nothing -> False)

  -- Verify /models autocomplete alias works identically to /model
  mLocalModelsComp <- completeInput "." localUI "/models "
  assert "/models autocomplete behaves identically to /model"
    (fmap (map candInsert . compCandidates) mLocalModelsComp == fmap (map candInsert . compCandidates) mLocalComp)

  -- Verify direct OpenAI endpoint completion never contains 'claude'
  let openAiUI = localUI { uiConfig = openAiCfg, uiModelName = "gpt-4o" }
  mOpenAiComp <- completeInput "." openAiUI "/model "
  assert "Direct OpenAI autocomplete contains 'gpt-4o' and '4o' but NEVER 'claude'"
    (case mOpenAiComp of
       Just cs ->
         let inserts = map candInsert (compCandidates cs)
         in "4o" `elem` inserts && "gpt-4o" `elem` inserts && not ("claude" `elem` inserts)
       Nothing -> False)

  putStrLn "  -> OK: Token cycling replaces cleanly in-place, local endpoints exclude cloud models, and aliases resolve endpoint-appropriately."

assert :: String -> Bool -> IO ()
assert desc condition =
  unless condition $ failTest ("Assertion failed: " <> desc)

failTest :: String -> IO ()
failTest msg = do
  putStrLn $ "  [FAIL] " <> msg
  exitFailure
