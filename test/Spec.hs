{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (removeDirectoryRecursive, doesFileExist, removeFile)
import System.Environment (setEnv, unsetEnv)
import System.Exit (exitFailure)

import Lambda.Config (loadConfig, Config(..))
import Lambda.Core.ToolProvider
import Lambda.Engine.Artifacts
import Lambda.Engine.Compactor
import Lambda.Engine.Security
import qualified Data.Map.Strict as Map
import Brick.Types (vSize, Size(..))
import Lambda.Provider.Builtin (listDirectoryTool, readFileTool, fetchUrlTool)
import Lambda.Provider.Mcp (inferCapability, parseMcpCallResult, startAndLoadMcpServers, stopMcpClient)
import Lambda.UI.Draw (renderSubAgents)
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
        , subAgentHypothesis = "Test hypothesis for ASan crash"
        , subAgentTurnCount = 2
        , subAgentBudget = 10
        , subAgentStatus = SubAgentRunning
        , subAgentArtifact = Just ".lambda/artifacts/subagent_1.log"
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

assert :: String -> Bool -> IO ()
assert desc condition =
  unless condition $ failTest ("Assertion failed: " <> desc)

failTest :: String -> IO ()
failTest msg = do
  putStrLn $ "  [FAIL] " <> msg
  exitFailure
