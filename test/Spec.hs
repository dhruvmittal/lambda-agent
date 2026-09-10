{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (removeDirectoryRecursive, doesFileExist)
import System.Environment (setEnv, unsetEnv)
import System.Exit (exitFailure)

import Lambda.Config (loadConfig, Config(..))
import Lambda.Core.ToolProvider
import Lambda.Engine.Artifacts
import Lambda.Engine.Compactor
import Lambda.Engine.Security
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
  let t1Wire = head wireMessages
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

  assert "Generated 2 wire messages" (length wire == 2)
  let asstJson = TE.decodeUtf8 (BL.toStrict (Aeson.encode (head wire)))
      toolJson = TE.decodeUtf8 (BL.toStrict (Aeson.encode (last wire)))

  assert "Assistant message has role: assistant" ("\"role\":\"assistant\"" `T.isInfixOf` asstJson)
  assert "Assistant message has native tool_calls array" ("\"tool_calls\":" `T.isInfixOf` asstJson)
  assert "Assistant message has tool call ID" ("call_abc123" `T.isInfixOf` asstJson)
  assert "Tool response has role: tool" ("\"role\":\"tool\"" `T.isInfixOf` toolJson)
  assert "Tool response references tool_call_id" ("\"tool_call_id\":\"call_abc123\"" `T.isInfixOf` toolJson)
  putStrLn "  -> OK: Native OpenAI structured tool calls and tool responses formatted cleanly."

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

assert :: String -> Bool -> IO ()
assert desc condition =
  unless condition $ failTest ("Assertion failed: " <> desc)

failTest :: String -> IO ()
failTest msg = do
  putStrLn $ "  [FAIL] " <> msg
  exitFailure
