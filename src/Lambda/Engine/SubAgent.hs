{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.SubAgent
  ( SubAgentSpec(..)
  , SubAgentResult(..)
  , runEphemeralSubAgent
  , spawnSubAgentTool
  ) where

import Control.Concurrent.STM
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider (ToolDefinition(..), toolsToOpenAISchema)
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.Security (requestGrantApproval)
import Lambda.Engine.State
import Lambda.Types

data SubAgentSpec = SubAgentSpec
  { subTaskDescription :: !Text
  , subTurnBudget      :: !Int
  , subGrantedGlobs    :: ![Text]
  }

data SubAgentResult = SubAgentResult
  { subStatus     :: !Text
  , subRootCause  :: !Text
  , subEvidence   :: !Text
  , subRemedy     :: !Text
  , subLogPath    :: !(Maybe FilePath)
  } deriving (Eq, Show)

-- | Spawns and executes an ephemeral subagent with isolated STM context and bounded budget
runEphemeralSubAgent
  :: AppEngineState
  -> ModelDriver
  -> SubAgentSpec
  -> IO (Either Text SubAgentResult)
runEphemeralSubAgent engineState@AppEngineState{..} driver SubAgentSpec{..} = do
  task <- registerSubAgentTask engineState subTaskDescription subTurnBudget
  let sId = subAgentId task

  -- Step 1: Upfront proactive capability authorization
  authorized <- requestGrantApproval appSecurity sId subTaskDescription subGrantedGlobs
  if not authorized
    then do
      updateSubAgentStatus engineState sId (SubAgentBlocked "Capability grant denied by user")
      pure $ Left "Subagent launch aborted: Capability grant denied by user."
    else do
      -- Step 2: Isolated ephemeral state
      subTurnsVar <- newTVarIO
        [ Turn 1 SystemRole
            [ TextBlock $ T.unlines
                [ "# Identity: lambdA Ephemeral Diagnostic Worker"
                , "You are an ephemeral diagnostic sub-agent spawned for a specific investigation."
                , "Task: " <> subTaskDescription
                , "Operate within your turn budget. When diagnosis is complete, output a structured block:"
                , "```diagnosis"
                , "STATUS: SUCCESS | INCONCLUSIVE | BLOCKED"
                , "ROOT_CAUSE: <explanation>"
                , "EVIDENCE: <details>"
                , "RECOMMENDED_REMEDY: <fix>"
                , "```"
                ]
            ]
        , Turn 2 UserRole [TextBlock ("Begin investigation: " <> subTaskDescription)]
        ]

      -- Step 3: Run execution loop
      res <- subAgentLoop engineState driver sId subGrantedGlobs subTurnsVar 1 subTurnBudget
      case res of
        Right r -> do
          updateSubAgentStatus engineState sId (SubAgentSuccess (subStatus r))
          pure (Right r)
        Left err -> do
          updateSubAgentStatus engineState sId (SubAgentFailed err)
          pure (Left err)

subAgentLoop
  :: AppEngineState
  -> ModelDriver
  -> Int
  -> [Text]
  -> TVar [Turn]
  -> Int
  -> Int
  -> IO (Either Text SubAgentResult)
subAgentLoop engineState@AppEngineState{..} driver sId grants turnsVar currentTurn budget
  | currentTurn > budget =
      pure $ Right $ SubAgentResult "INCONCLUSIVE" "Turn budget exhausted without definitive root cause." "" "" Nothing
  | otherwise = do
      turns <- readTVarIO turnsVar
      reg <- readTVarIO appToolRegistry
      let toolSchemas = toolsToOpenAISchema ExecMode reg

      accumTextVar <- newTVarIO ("" :: Text)
      accumThinkingVar <- newTVarIO ("" :: Text)
      toolCallsVar <- newTVarIO ([] :: [ToolCall])

      -- Stream LLM response
      streamCompletion driver turns toolSchemas $ \case
        ChunkText txt -> atomically $ modifyTVar' accumTextVar (<> txt)
        ChunkThinking th -> atomically $ modifyTVar' accumThinkingVar (<> th)
        ChunkToolCallStart cid name -> atomically $
          modifyTVar' toolCallsVar (\tcs -> tcs ++ [ToolCall cid name (Aeson.object [])])
        ChunkToolCallArgs cid argsChunk -> atomically $
          modifyTVar' toolCallsVar (\tcs -> map (appendArgs cid argsChunk) tcs)
        ChunkDone -> pure ()

      fullText <- readTVarIO accumTextVar
      fullThinking <- readTVarIO accumThinkingVar
      toolCalls <- readTVarIO toolCallsVar

      let assistantBlocks =
            [ ThinkingBlock currentTurn fullThinking Visible | not (T.null fullThinking) ]
            ++ [ TextBlock fullText | not (T.null fullText) ]
            ++ map ToolCallBlock toolCalls

      let newAssistantTurn = Turn (currentTurn * 2 + 1) AssistantRole assistantBlocks
      atomically $ modifyTVar' turnsVar (\ts -> ts ++ [newAssistantTurn])

      -- Check for diagnosis block in text
      case parseDiagnosis fullText of
        Just result -> pure $ Right result
        Nothing -> do
          if null toolCalls
            then
              pure $ Right $ SubAgentResult "COMPLETED" fullText "" "" Nothing
            else do
              -- Execute tool calls in subagent context
              toolResults <- mapM (executeToolDispatch engineState (SubAgentId sId subTaskDescription) grants) toolCalls
              let toolTurn = Turn (currentTurn * 2 + 2) ToolRole (map ToolResultBlock toolResults)
              atomically $ modifyTVar' turnsVar (\ts -> ts ++ [toolTurn])
              subAgentLoop engineState driver sId grants turnsVar (currentTurn + 1) budget
  where
    subTaskDescription = "SubAgent #" <> T.pack (show sId)
    appendArgs targetId chunk tc@ToolCall{..}
      | toolCallId == targetId =
          let existing = case toolCallArgs of
                Aeson.String s -> s
                _              -> ""
          in tc { toolCallArgs = Aeson.String (existing <> chunk) }
      | otherwise = tc

parseDiagnosis :: Text -> Maybe SubAgentResult
parseDiagnosis txt
  | "```diagnosis" `T.isInfixOf` txt =
      let ls = T.lines txt
          extractField prefix =
            case filter (prefix `T.isPrefixOf`) ls of
              (l:_) -> T.strip $ T.drop (T.length prefix) l
              []    -> ""
      in Just SubAgentResult
          { subStatus    = extractField "STATUS:"
          , subRootCause = extractField "ROOT_CAUSE:"
          , subEvidence  = extractField "EVIDENCE:"
          , subRemedy    = extractField "RECOMMENDED_REMEDY:"
          , subLogPath   = Nothing
          }
  | otherwise = Nothing

-- | Built-in tool exposed to the primary agent to spawn ephemeral workers
spawnSubAgentTool :: AppEngineState -> ModelDriver -> ToolDefinition
spawnSubAgentTool engineState driver = ToolDefinition
  { toolName = "spawn_diagnostic_subagent"
  , toolDescription = "Spawn an ephemeral subagent to execute an isolated multi-step diagnostic or debugging task. Elides raw tool outputs from the main context."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "task" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Hypothesis or diagnostic goal for the subagent." :: Text)
              ]
          , "budget" .= Aeson.object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Maximum number of turns allowed (default 6)." :: Text)
              ]
          , "granted_capabilities" .= Aeson.object
              [ "type" .= ("array" :: Text)
              , "items" .= Aeson.object [ "type" .= ("string" :: Text) ]
              , "description" .= ("List of glob patterns the subagent is authorized to run (e.g. ['ctest*', 'gdb*', 'read_file*'])." :: Text)
              ]
          ]
      , "required" .= (["task", "granted_capabilities"] :: [Text])
      ]
  , toolCapability = Destructive
  , toolExecute = \_caller args -> do
      case parseEither parseArgs args of
        Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
        Right (task, mBudget, grants) -> do
          let budget = maybe 6 id mBudget
              spec = SubAgentSpec task budget grants
          res <- runEphemeralSubAgent engineState driver spec
          case res of
            Left err -> pure $ ToolResult "" "" ("SubAgent failed: " <> err) Nothing
            Right SubAgentResult{..} -> do
              let summary = T.unlines
                    [ "<subagent_diagnosis status=\"" <> subStatus <> "\">"
                    , "  ROOT_CAUSE: " <> subRootCause
                    , "  EVIDENCE: " <> subEvidence
                    , "  REMEDY: " <> subRemedy
                    , "</subagent_diagnosis>"
                    ]
              pure $ ToolResult "" summary "" subLogPath
  }
  where
    parseArgs = Aeson.withObject "spawn" $ \o -> do
      t <- o .: "task"
      b <- o .:? "budget"
      g <- o .: "granted_capabilities"
      pure (t, b, g)
