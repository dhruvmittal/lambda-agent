{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.SubAgent
  ( SubAgentSpec(..)
  , SubAgentReport(..)
  , SubAgentResult
  , runEphemeralSubAgent
  , resumeEphemeralSubAgent
  , spawnSpecialistSubAgentTool
  , resumeSpecialistSubAgentTool
  , submitReportTool
  , filterSubAgentRegistry
  , subAgentLoop
  , accumulateStreamChunks
  , appendArgs
  , finalizeToolArgs
  ) where

import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.STM
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

import Lambda.Config (Config(..), SpecialistConfig(..))
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider (ToolDefinition(..), ToolRegistry(..), toolsToOpenAISchema)
import Lambda.Engine.Dispatcher (executeToolDispatch)
import Lambda.Engine.State
import Lambda.Types

data SubAgentSpec = SubAgentSpec
  { subAgentRoleName   :: !Text
  , subTaskDescription :: !Text
  , subTurnBudget      :: !Int
  , subGrantedGlobs    :: ![Text]
  } deriving (Eq, Show)

data SubAgentReport = SubAgentReport
  { reportStatus   :: !Text
  , reportSummary  :: !Text
  , reportDetails  :: !Text
  , reportArtifact :: !(Maybe Text)
  } deriving (Eq, Show)

type SubAgentResult = SubAgentReport

-- | Synthetic report egress tool injected into every subagent's toolset
submitReportTool :: TVar (Maybe SubAgentReport) -> ToolDefinition
submitReportTool reportVar = ToolDefinition
  { toolName = "submit_report"
  , toolDescription = "Submit the specialist deliverable and conclude execution. Required for all specialist subagents."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "status" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "enum" .= (["SUCCESS", "INCONCLUSIVE", "BLOCKED"] :: [Text])
              , "description" .= ("Outcome status of the assigned task." :: Text)
              ]
          , "summary" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("High-level summary of findings, diagnosis, or results." :: Text)
              ]
          , "details" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Detailed evidence, test traces, benchmark numbers, or diff analysis." :: Text)
              ]
          , "artifact_path" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Optional path to generated artifact, test log, or diff file." :: Text)
              ]
          ]
      , "required" .= (["status", "summary", "details"] :: [Text])
      ]
  , toolCapability = ReadOnly
  , toolIsMcp = False
  , toolExecute = \_caller args -> do
      case parseEither parseReport args of
        Left err -> pure $ ToolResult "" "" ("Invalid submit_report arguments: " <> T.pack err) Nothing
        Right r -> do
          atomically $ writeTVar reportVar (Just r)
          pure $ ToolResult "" "Report accepted. Specialist task concluding." "" (fmap T.unpack (reportArtifact r))
  }
  where
    parseReport = Aeson.withObject "submit_report" $ \o -> do
      s <- o .: "status"
      sum' <- o .: "summary"
      det <- o .: "details"
      art <- o .:? "artifact_path"
      pure $ SubAgentReport s sum' det art

-- | Filters out any recursive spawn and resume tools to strictly enforce a Depth-1 Star Graph,
-- and injects the synthetic submitReportTool.
filterSubAgentRegistry :: ToolDefinition -> ToolRegistry -> ToolRegistry
filterSubAgentRegistry repTool (ToolRegistry m) =
  let withoutSubControl = Map.filterWithKey (\k _ -> not ("spawn_" `T.isPrefixOf` k || "resume_" `T.isPrefixOf` k)) m
      withReport = Map.insert (toolName repTool) repTool withoutSubControl
  in ToolRegistry withReport

-- | Spawns and executes an ephemeral specialist subagent with isolated STM context and bounded budget
runEphemeralSubAgent
  :: AppEngineState
  -> ModelDriver
  -> SubAgentSpec
  -> IO (Either Text SubAgentReport)
runEphemeralSubAgent engineState@AppEngineState{..} driver SubAgentSpec{..} = do
  turns <- readTVarIO appTurns
  let parentTurnId = if null turns then Nothing else Just (turnId (last turns))
  task <- registerSubAgentTaskWithParent engineState subAgentRoleName subTaskDescription subTurnBudget parentTurnId
  let sId = subAgentId task

  curMode <- readTVarIO appMode

  -- Look up specialist persona in configuration
  let mSpecialist = Map.lookup (T.toLower subAgentRoleName) (specialists appConfig)
      specPrompt = case mSpecialist of
        Just s  -> specialistPrompt s
        Nothing -> "# Identity: Specialist SubAgent [" <> subAgentRoleName <> "]\nYou are an ephemeral specialist subagent for an isolated task."
      defaultCaps = maybe [] specialistCapabilities mSpecialist
      defaultBud = maybe 6 specialistBudget mSpecialist
      effectiveBudget = if subTurnBudget > 0 then subTurnBudget else defaultBud
      combinedGlobs = if null subGrantedGlobs then defaultCaps else nub (defaultCaps ++ subGrantedGlobs)

  -- Step 1: Capability bounds
  -- In PlanMode, filter out any destructive grants so subagent is strictly read-only
  let isDestructiveGlob g = any (`T.isPrefixOf` g) ["bash*", "write_file*", "edit_file*", "replace_lines*"]
      effectiveGrants = if curMode == PlanMode
        then filter (not . isDestructiveGlob) combinedGlobs
        else combinedGlobs
      grantedWithReport = "submit_report*" : effectiveGrants

  -- Step 2: Isolated ephemeral state with Depth-1 Star Graph enforcement
  reportVar <- newTVarIO Nothing
  attemptedToolsVar <- newTVarIO ([] :: [Text])

  baseReg <- readTVarIO appToolRegistry
  let subReg = filterSubAgentRegistry (submitReportTool reportVar) baseReg
  subRegVar <- newTVarIO subReg
  let subEngineState = engineState { appToolRegistry = subRegVar }

  let sysPrompt = T.unlines
        [ specPrompt
        , ""
        , "## Operating Mode: " <> (if curMode == PlanMode then "[PLAN MODE - Read-Only]" else "[EXEC MODE - Mutating]")
        , "## Assigned Objective: " <> subTaskDescription
        , ""
        , "## Reporting Invariant:"
        , "Operate within your turn budget. You have access to the synthetic tool `submit_report`."
        , "When your work, survey, diagnosis, profiling, or implementation is complete, you MUST call `submit_report` with status, summary, and details."
        ]
      initTurns =
        [ Turn 1 SystemRole [TextBlock sysPrompt]
        , Turn 2 UserRole [TextBlock ("Begin task: " <> subTaskDescription)]
        ]
  subTurnsVar <- newTVarIO initTurns
  updateSubAgentTurns engineState sId initTurns

  -- Step 3: Run execution loop
  res <- subAgentLoop subEngineState driver sId grantedWithReport subTurnsVar attemptedToolsVar reportVar 1 effectiveBudget
  case res of
    Right r -> do
      updateSubAgentStatus engineState sId (SubAgentSuccess (reportStatus r))
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
  -> TVar [Text]
  -> TVar (Maybe SubAgentReport)
  -> Int
  -> Int
  -> IO (Either Text SubAgentReport)
subAgentLoop engineState@AppEngineState{..} driver sId grants turnsVar attemptedToolsVar reportVar currentTurn budget
  | currentTurn > budget = do
      mReport <- readTVarIO reportVar
      case mReport of
        Just rep -> pure $ Right rep
        Nothing -> do
          attempted <- readTVarIO attemptedToolsVar
          let attemptedSummary = if null attempted
                then "none"
                else T.intercalate ", " (reverse attempted)
              failRep = SubAgentReport
                { reportStatus   = "INCONCLUSIVE"
                , reportSummary  = "Turn budget exhausted after " <> T.pack (show budget) <> " turns without submitting a report."
                , reportDetails  = "Attempted tools: [" <> attemptedSummary <> "]"
                , reportArtifact = Nothing
                }
          pure $ Right failRep
  | otherwise = do
      turns <- readTVarIO turnsVar
      reg <- readTVarIO appToolRegistry
      mode <- readTVarIO appMode
      let toolSchemas = toolsToOpenAISchema mode reg

      (fullText, fullThinking, toolCalls, wasInterrupted) <-
        accumulateStreamChunks appInterrupted driver turns toolSchemas (\_ _ -> pure ()) (\_ -> pure ())

      if wasInterrupted
        then pure $ Left "Subagent execution cancelled by user."
        else do
          let assistantBlocks =
                [ ThinkingBlock currentTurn fullThinking Visible | not (T.null fullThinking) ]
                ++ [ TextBlock fullText | not (T.null fullText) ]
                ++ map ToolCallBlock toolCalls

          turnsNow <- readTVarIO turnsVar
          let nextAsstId = if null turnsNow then 1 else maximum (map turnId turnsNow) + 1
              newAssistantTurn = Turn nextAsstId AssistantRole assistantBlocks
          asstTurns <- atomically $ do
            modifyTVar' turnsVar (\ts -> ts ++ [newAssistantTurn])
            readTVar turnsVar
          updateSubAgentTurns engineState sId asstTurns

          if null toolCalls
            then do
              mReport <- readTVarIO reportVar
              case mReport of
                Just rep -> pure $ Right rep
                Nothing  -> pure $ Right $ SubAgentReport "COMPLETED" fullText "" Nothing
            else do
              -- Record attempted tools
              let formatCall tc =
                    let argSummary = case toolCallArgs tc of
                          Aeson.Object o ->
                            let parseArgs = parseEither (\obj -> do
                                  c <- obj .:? "command"
                                  p <- obj .:? "path"
                                  q <- obj .:? "query"
                                  pat <- obj .:? "pattern"
                                  pure (c, p, q, pat)
                                  ) o
                            in case parseArgs of
                                 Right (Just (c :: Text), _, _, _) -> "(" <> c <> ")"
                                 Right (_, Just (p :: Text), _, _) -> "(" <> p <> ")"
                                 Right (_, _, Just (q :: Text), _) -> "(" <> q <> ")"
                                 Right (_, _, _, Just (pat :: Text)) -> "(" <> pat <> ")"
                                 _ -> "()"
                          _ -> "()"
                    in toolCallName tc <> argSummary
              atomically $ modifyTVar' attemptedToolsVar (\prev -> map formatCall toolCalls ++ prev)

              -- Execute tool calls concurrently in subagent context with interrupt checking
              toolResults <- mapConcurrently (runSingleSubTool (SubAgentId sId subTaskDescription)) toolCalls
              turnsAfterAsst <- readTVarIO turnsVar
              let nextToolId = if null turnsAfterAsst then 2 else maximum (map turnId turnsAfterAsst) + 1
                  toolTurn = Turn nextToolId ToolRole (map ToolResultBlock toolResults)
              toolTurns <- atomically $ do
                modifyTVar' turnsVar (\ts -> ts ++ [toolTurn])
                readTVar turnsVar
              updateSubAgentTurns engineState sId toolTurns

              stillActive <- not <$> readTVarIO appInterrupted
              if not stillActive
                then pure $ Left "Subagent execution cancelled by user."
                else do
                  mReport <- readTVarIO reportVar
                  case mReport of
                    Just rep -> pure $ Right rep
                    Nothing  -> subAgentLoop engineState driver sId grants turnsVar attemptedToolsVar reportVar (currentTurn + 1) budget
  where
    subTaskDescription = "SubAgent #" <> T.pack (show sId)

    runSingleSubTool caller tc = do
      res <- race
        (atomically $ do
          intr <- readTVar appInterrupted
          check intr)
        (executeToolDispatch engineState caller grants tc)
      case res of
        Left () -> pure $ ToolResult (toolCallId tc) "" "Subagent tool execution cancelled by user." Nothing
        Right r -> pure r

-- | Shared low-level stream chunk accumulator for orchestrator and subagents
accumulateStreamChunks
  :: TVar Bool
  -> ModelDriver
  -> [Turn]
  -> [Aeson.Value]
  -> (Text -> Text -> IO ())
  -> (StreamChunk -> IO ())
  -> IO (Text, Text, [ToolCall], Bool)
accumulateStreamChunks intrVar driver turns toolSchemas onProgress onChunk = do
  accumTextVar <- newTVarIO ("" :: Text)
  accumThinkingVar <- newTVarIO ("" :: Text)
  toolCallsVar <- newTVarIO ([] :: [ToolCall])

  streamCompletion driver turns toolSchemas $ \chunk -> do
    intr <- readTVarIO intrVar
    unless intr $ do
      onChunk chunk
      case chunk of
        ChunkText txt -> do
          atomically $ modifyTVar' accumTextVar (<> txt)
          t <- readTVarIO accumTextVar
          th <- readTVarIO accumThinkingVar
          onProgress t th
        ChunkThinking th -> do
          atomically $ modifyTVar' accumThinkingVar (<> th)
          t <- readTVarIO accumTextVar
          th' <- readTVarIO accumThinkingVar
          onProgress t th'
        ChunkToolCallStart cid name -> atomically $
          modifyTVar' toolCallsVar (\tcs -> tcs ++ [ToolCall cid name (Aeson.String "")])
        ChunkToolCallArgs cid argsChunk -> atomically $
          modifyTVar' toolCallsVar (appendArgs cid argsChunk)
        ChunkDone -> pure ()

  fullText <- readTVarIO accumTextVar
  fullThinking <- readTVarIO accumThinkingVar
  rawToolCalls <- readTVarIO toolCallsVar
  wasInterrupted <- readTVarIO intrVar
  pure (fullText, fullThinking, map finalizeToolArgs rawToolCalls, wasInterrupted)

appendArgs :: Text -> Text -> [ToolCall] -> [ToolCall]
appendArgs targetId chunk tcs
  | T.null targetId =
      if null tcs
        then []
        else let (prev, lastTc) = (init tcs, last tcs)
             in prev ++ [addChunk chunk lastTc]
  | "idx_" `T.isPrefixOf` targetId =
      case (readMaybe (T.unpack (T.drop 4 targetId)) :: Maybe Int) of
        Just idx | idx >= 0 && idx < length tcs ->
          case splitAt idx tcs of
            (before, target : after) -> before ++ [addChunk chunk target] ++ after
            _ -> map (\tc -> if toolCallId tc == targetId then addChunk chunk tc else tc) tcs
        _ -> map (\tc -> if toolCallId tc == targetId then addChunk chunk tc else tc) tcs
  | otherwise =
      map (\tc -> if toolCallId tc == targetId then addChunk chunk tc else tc) tcs
  where
    addChunk ch tc =
      let existing = case toolCallArgs tc of
            Aeson.String s -> s
            _              -> ""
      in tc { toolCallArgs = Aeson.String (existing <> ch) }

finalizeToolArgs :: ToolCall -> ToolCall
finalizeToolArgs tc@ToolCall{..} =
  case toolCallArgs of
    Aeson.String s
      | T.null (T.strip s) -> tc { toolCallArgs = Aeson.object [] }
      | otherwise ->
          case Aeson.decode (BL.fromStrict $ TE.encodeUtf8 s) of
            Just val -> tc { toolCallArgs = val }
            Nothing  -> tc { toolCallArgs = Aeson.object ["raw" .= s] }
    _ -> tc

-- | Tool to spawn specialist subagents
spawnSpecialistSubAgentTool :: AppEngineState -> ModelDriver -> ToolDefinition
spawnSpecialistSubAgentTool engineState driver = ToolDefinition
  { toolName = "spawn_specialist_subagent"
  , toolDescription = "Spawn an ephemeral specialist subagent to execute an isolated task (e.g. surveyor for codebase mapping, debugger for failure analysis, profiler for performance, implementer for code changes, reviewer for adversarial critique). Subagents run isolated and report via submit_report."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "role" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Specialist role: surveyor, debugger, profiler, implementer, reviewer, or any specialist configured in config.specialists." :: Text)
              ]
          , "task" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Hypothesis, survey goal, or task description for the specialist." :: Text)
              ]
          , "budget" .= Aeson.object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Optional maximum turns allowed (defaults to specialist configured budget)." :: Text)
              ]
          , "granted_capabilities" .= Aeson.object
              [ "type" .= ("array" :: Text)
              , "items" .= Aeson.object [ "type" .= ("string" :: Text) ]
              , "description" .= ("Optional list of capability glob patterns (e.g. ['read_file*', 'grep_search*', 'sd_*'])." :: Text)
              ]
          ]
      , "required" .= (["role", "task"] :: [Text])
      ]
  , toolCapability = ReadOnly
  , toolIsMcp = False
  , toolExecute = \_caller args -> do
      case parseEither parseSpecialistArgs args of
        Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
        Right (role, task, mBudget, mGrants) -> do
          let budget = maybe 0 id mBudget
              grants = maybe [] id mGrants
              spec = SubAgentSpec role task budget grants
          res <- runEphemeralSubAgent engineState driver spec
          case res of
            Left err -> pure $ ToolResult "" "" ("Specialist subagent failed: " <> err) Nothing
            Right SubAgentReport{..} -> do
              let artifactAttr = maybe "" (\a -> " artifact=\"" <> a <> "\"") reportArtifact
                  summary = T.unlines
                    [ "<specialist_report role=\"" <> role <> "\" status=\"" <> reportStatus <> "\"" <> artifactAttr <> ">"
                    , "  <summary>" <> reportSummary <> "</summary>"
                    , "  <details>" <> reportDetails <> "</details>"
                    , "</specialist_report>"
                    ]
              pure $ ToolResult "" summary "" (fmap T.unpack reportArtifact)
  }
  where
    parseSpecialistArgs = Aeson.withObject "spawn_specialist" $ \o -> do
      r <- o .: "role"
      t <- o .: "task"
      b <- o .:? "budget"
      g <- o .:? "granted_capabilities"
      pure (r, t, b, g)

-- | Resumes or specializes an existing subagent with preserved conversation history
resumeEphemeralSubAgent
  :: AppEngineState
  -> ModelDriver
  -> Int               -- ^ sId
  -> Int               -- ^ additionalBudget
  -> Maybe Text        -- ^ mNewRole
  -> Maybe Text        -- ^ mGuidance
  -> [Text]            -- ^ additionalGrants
  -> IO (Either Text SubAgentReport)
resumeEphemeralSubAgent engineState@AppEngineState{..} driver sId addBudget mNewRole mGuidance additionalGrants = do
  subs <- readTVarIO appSubAgents
  case Map.lookup sId subs of
    Nothing -> pure $ Left $ "SubAgent #" <> T.pack (show sId) <> " not found."
    Just task
      | subAgentStatus task == SubAgentRunning ->
          pure $ Left $ "SubAgent #" <> T.pack (show sId) <> " is already running."
      | otherwise -> do
          curMode <- readTVarIO appMode
          let currentRole = subAgentRole task
              effectiveRole = case mNewRole of
                Just r | not (T.null (T.strip r)) -> T.strip r
                _ -> currentRole
              isSpecialized = effectiveRole /= currentRole
              mSpecialist = Map.lookup (T.toLower effectiveRole) (specialists appConfig)
              specPrompt = case mSpecialist of
                Just s  -> specialistPrompt s
                Nothing -> "# Identity: Specialist SubAgent [" <> effectiveRole <> "]\nYou are an ephemeral specialist subagent for an isolated task."
              defaultCaps = maybe [] specialistCapabilities mSpecialist
              effectiveAddBudget = if addBudget > 0 then addBudget else maybe 6 specialistBudget mSpecialist
              newTotalBudget = subAgentBudget task + effectiveAddBudget
              combinedGlobs = nub (defaultCaps ++ additionalGrants)
              isDestructiveGlob g = any (`T.isPrefixOf` g) ["bash*", "write_file*", "edit_file*", "replace_lines*"]
              effectiveGrants = if curMode == PlanMode
                then filter (not . isDestructiveGlob) combinedGlobs
                else combinedGlobs
              grantedWithReport = "submit_report*" : effectiveGrants

          -- Prepare continuation prompt
          let continuationLines =
                [ "## Task Resumed"
                , "Your turn budget has been extended by " <> T.pack (show effectiveAddBudget) <> " turns (total budget: " <> T.pack (show newTotalBudget) <> " turns)."
                ]
                ++ [ "## Role Specialization: " <> effectiveRole | isSpecialized ]
                ++ [ specPrompt | isSpecialized ]
                ++ [ "## Additional Guidance:\n" <> g | Just g <- [mGuidance], not (T.null (T.strip g)) ]
                ++ [ "Continue your investigation and execute `submit_report` when your task is complete." ]
              continuationText = T.unlines continuationLines
              oldTurns = subAgentTurns task
              nextTurnId = if null oldTurns then 1 else maximum (map turnId oldTurns) + 1
              continuationTurn = Turn nextTurnId UserRole [TextBlock continuationText]
              updatedInitTurns = oldTurns ++ [continuationTurn]

          -- Update task in state vector
          _ <- updateSubAgentTask engineState sId $ \t -> t
            { subAgentRole = effectiveRole
            , subAgentBudget = newTotalBudget
            , subAgentStatus = SubAgentRunning
            , subAgentTurns = updatedInitTurns
            }

          -- Setup execution loop with isolated registry
          reportVar <- newTVarIO Nothing
          attemptedToolsVar <- newTVarIO ([] :: [Text])
          baseReg <- readTVarIO appToolRegistry
          let subReg = filterSubAgentRegistry (submitReportTool reportVar) baseReg
          subRegVar <- newTVarIO subReg
          let subEngineState = engineState { appToolRegistry = subRegVar }
          subTurnsVar <- newTVarIO updatedInitTurns

          let asstCount = length (filter (\t -> turnRole t == AssistantRole) oldTurns)
              startTurn = asstCount + 1

          res <- subAgentLoop subEngineState driver sId grantedWithReport subTurnsVar attemptedToolsVar reportVar startTurn newTotalBudget
          case res of
            Right r -> do
              updateSubAgentStatus engineState sId (SubAgentSuccess (reportStatus r))
              pure (Right r)
            Left err -> do
              updateSubAgentStatus engineState sId (SubAgentFailed err)
              pure (Left err)

-- | Tool to resume or specialize an existing subagent
resumeSpecialistSubAgentTool :: AppEngineState -> ModelDriver -> ToolDefinition
resumeSpecialistSubAgentTool engineState driver = ToolDefinition
  { toolName = "resume_subagent"
  , toolDescription = "Resume or specialize an existing subagent to continue an incomplete task or pivot to a new role without re-running earlier steps. Re-uses all previous turns, files read, and tool observations."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "subagent_id" .= Aeson.object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("ID of the existing subagent to resume (e.g. 1)." :: Text)
              ]
          , "additional_budget" .= Aeson.object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Optional additional turns granted (defaults to 6 or specialist configured budget)." :: Text)
              ]
          , "guidance" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Optional guidance, focus area, or instructions for continuing the task." :: Text)
              ]
          , "specialize_role" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Optional specialist role to pivot into (e.g. 'implementer', 'debugger', 'reviewer'). Updates persona and capabilities." :: Text)
              ]
          , "granted_capabilities" .= Aeson.object
              [ "type" .= ("array" :: Text)
              , "items" .= Aeson.object [ "type" .= ("string" :: Text) ]
              , "description" .= ("Optional additional capability glob patterns to grant (e.g. ['edit_file*', 'bash*'])." :: Text)
              ]
          ]
      , "required" .= (["subagent_id"] :: [Text])
      ]
  , toolCapability = ReadOnly
  , toolIsMcp = False
  , toolExecute = \_caller args -> do
      case parseEither parseResumeArgs args of
        Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
        Right (sId, mBudget, mGuidance, mRole, mGrants) -> do
          let budget = maybe 0 id mBudget
              grants = maybe [] id mGrants
          res <- resumeEphemeralSubAgent engineState driver sId budget mRole mGuidance grants
          case res of
            Left err -> pure $ ToolResult "" "" ("Failed to resume subagent #" <> T.pack (show sId) <> ": " <> err) Nothing
            Right SubAgentReport{..} -> do
              let artifactAttr = maybe "" (\a -> " artifact=\"" <> a <> "\"") reportArtifact
                  effectiveRole = maybe "specialist" id mRole
                  summary = T.unlines
                    [ "<specialist_report role=\"" <> effectiveRole <> "\" status=\"" <> reportStatus <> "\"" <> artifactAttr <> ">"
                    , "  <summary>" <> reportSummary <> "</summary>"
                    , "  <details>" <> reportDetails <> "</details>"
                    , "</specialist_report>"
                    ]
              pure $ ToolResult "" summary "" (fmap T.unpack reportArtifact)
  }
  where
    parseResumeArgs = Aeson.withObject "resume_subagent" $ \o -> do
      sId <- o .: "subagent_id"
      b <- o .:? "additional_budget"
      g <- o .:? "guidance"
      r <- o .:? "specialize_role"
      gr <- o .:? "granted_capabilities"
      pure (sId, b, g, r, gr)
