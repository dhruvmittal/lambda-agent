{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.Engine.SubAgent
  ( SubAgentSpec(..)
  , SubAgentReport(..)
  , SubAgentResult
  , SubAgentRegistry(..)
  , runEphemeralSubAgent
  , runEphemeralSubAgentWithCallId
  , resumeEphemeralSubAgent
  , resumeEphemeralSubAgentWithCallId
  , spawnSpecialistSubAgentTool
  , resumeSpecialistSubAgentTool
  , formatSpecialistCatalog
  , formatSpecialistReportXml
  , submitReportTool
  , filterSubAgentRegistry
  , subAgentLoop
  , compactSubAgentTurns
  , accumulateStreamChunks
  , appendArgs
  , finalizeToolArgs
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (AsyncCancelled, mapConcurrently, waitCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, catch, displayException, fromException, throwIO)
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?), (.!=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

import Lambda.Config (Config(..), SpecialistConfig(..), lookupModelContextLimitWithConfig)
import Lambda.Core.ModelDriver (ModelDriver(..))
import Lambda.Core.ToolProvider (ToolDefinition(..), ToolRegistry(..), ToolScope(..), toolsToOpenAISchema)
import Lambda.Engine.Compactor (compactHistory, estimateTotalTokens)
import Lambda.Engine.Dispatcher (executeToolDispatchWithPerms)
import Lambda.Engine.Security (matchGlob)
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
  , toolScope = Universal
  , toolExecute = \_caller _mCallId args -> do
      case parseEither parseReport args of
        Left err -> pure $ ToolResult "" "" ("Invalid submit_report arguments: " <> T.pack err) Nothing
        Right r -> do
          atomically $ writeTVar reportVar (Just r)
          pure $ ToolResult "" "Report accepted. Specialist task concluding." "" (fmap T.unpack (reportArtifact r))
  }
  where
    parseReport = Aeson.withObject "submit_report" $ \o -> do
      s <- o .:? "status" .!= "SUCCESS"
      mSum <- o .:? "summary"
      mRes <- o .:? "result"
      mRep <- o .:? "report"
      let sum' = fromMaybe "Task completed" (mSum <|> mRes <|> mRep)
      mDet <- o .:? "details"
      mContent <- o .:? "content"
      let det = fromMaybe "" (mDet <|> mContent)
      art <- o .:? "artifact_path"
      pure $ SubAgentReport s sum' det art

-- | An opaque, type-verified subagent tool registry that statically guarantees no RootOnly tools
-- (such as subagent recursion or session resumption) can ever be held by a subagent.
newtype SubAgentRegistry = SubAgentRegistry { unSubAgentRegistry :: ToolRegistry }

-- | Filters out any recursive spawn and resume tools to strictly enforce a Depth-1 Star Graph,
-- and injects the synthetic submitReportTool.
filterSubAgentRegistry :: ToolDefinition -> ToolRegistry -> ToolRegistry
filterSubAgentRegistry repTool (ToolRegistry m) =
  let withoutSubControl = Map.filterWithKey (\k t ->
        toolScope t == Universal
        && not ("spawn_" `T.isPrefixOf` k || "resume_" `T.isPrefixOf` k
             || "spawn_" `T.isPrefixOf` toolName t || "resume_" `T.isPrefixOf` toolName t)) m
      withReport = Map.insert (toolName repTool) repTool withoutSubControl
  in ToolRegistry withReport

-- | Remove tools whose name is explicitly denied by subagent deny globs (never removes submit_report)
filterRegistryByDeny :: Set.Set Text -> ToolRegistry -> ToolRegistry
filterRegistryByDeny denies (ToolRegistry m)
  | Set.null denies = ToolRegistry m
  | otherwise = ToolRegistry $ Map.filter (\t ->
      toolName t == "submit_report"
      || not (any (\g -> matchGlob g (toolName t)) (Set.toList denies))) m

-- | Shared lifecycle wrapper for running or resuming a subagent with per-subagent cancellation and artifact capture
executeSubAgentSession
  :: AppEngineState
  -> ModelDriver
  -> Int
  -> SubAgentPermissions
  -> TVar [Turn]
  -> Int
  -> Int
  -> IO (Either Text SubAgentReport)
executeSubAgentSession engineState@AppEngineState{..} driver sId subPerms subTurnsVar startTurn budget = do
  reportVar <- newTVarIO Nothing
  attemptedToolsVar <- newTVarIO ([] :: [Text])
  baseReg <- readTVarIO appToolRegistry
  let subReg = filterRegistryByDeny (sapDeny subPerms) (filterSubAgentRegistry (submitReportTool reportVar) baseReg)
  subRegVar <- newTVarIO subReg
  let subEngineState = engineState { appToolRegistry = subRegVar }

  res <- withAsync
    (subAgentLoop subEngineState driver sId subPerms subTurnsVar attemptedToolsVar reportVar startTurn budget)
    (\subAsync ->
      let subHandle = void subAsync
      in bracket
        (atomically $ modifyTVar' appSubAgentAsyncs (Map.insert sId subHandle))
        (\_ -> atomically $ modifyTVar' appSubAgentAsyncs (Map.update (\v -> if v == subHandle then Nothing else Just v) sId))
        (\_ -> do
          r <- waitCatch subAsync
          case r of
            Right out -> pure out
            Left ex -> case fromException ex :: Maybe AsyncCancelled of
              Just _ -> pure (Left "Cancelled by user")
              Nothing -> throwIO ex))
    `catch` \(e :: SomeException) -> do
      let st = case fromException e of
            Just (_ :: AsyncCancelled) -> SubAgentBlocked "Cancelled by user"
            Nothing                    -> SubAgentFailed (T.pack (displayException e))
      updateSubAgentStatus engineState sId st
      throwIO e
  case res of
    Right r -> do
      let finalStatus = if reportStatus r == "BLOCKED"
            then SubAgentBlocked (reportSummary r)
            else SubAgentSuccess (reportStatus r)
      _ <- updateSubAgentTask engineState sId $ \t -> t
        { subAgentStatus   = finalStatus
        , subAgentArtifact = fmap T.unpack (reportArtifact r) <|> subAgentArtifact t
        }
      pure (Right r)
    Left err -> do
      let finalStatus = if err == "Cancelled by user" || err == "Subagent execution cancelled by user."
            then SubAgentBlocked "Cancelled by user"
            else SubAgentFailed err
      updateSubAgentStatus engineState sId finalStatus
      pure (Left err)

-- | Formats the configured specialist catalog into a compact, high-signal string for tool schemas
formatSpecialistCatalog :: Map.Map Text SpecialistConfig -> Text
formatSpecialistCatalog specs =
  T.intercalate "; "
    [ name <> " (" <> specialistDescription sc <> ", budget: " <> T.pack (show (specialistBudget sc)) <> "t)"
    | (name, sc) <- Map.toAscList specs
    ]

-- | Escapes XML special characters for safe embedding in specialist report tags
escapeXml :: Text -> Text
escapeXml = T.concatMap $ \case
  '&'  -> "&amp;"
  '<'  -> "&lt;"
  '>'  -> "&gt;"
  '"'  -> "&quot;"
  '\'' -> "&apos;"
  c    -> T.singleton c

-- | Formats a SubAgentReport into a canonical <specialist_report> XML block
formatSpecialistReportXml :: Maybe Int -> Text -> SubAgentReport -> Text
formatSpecialistReportXml mSubId role SubAgentReport{..} =
  let idAttr       = maybe "" (\i -> " id=\"" <> T.pack (show i) <> "\"") mSubId
      artifactAttr = maybe "" (\a -> " artifact=\"" <> escapeXml a <> "\"") reportArtifact
  in T.unlines
    [ "<specialist_report" <> idAttr <> " role=\"" <> escapeXml role <> "\" status=\"" <> escapeXml reportStatus <> "\"" <> artifactAttr <> ">"
    , "  <summary>" <> escapeXml reportSummary <> "</summary>"
    , "  <details>" <> escapeXml reportDetails <> "</details>"
    , "</specialist_report>"
    ]

-- | Shared specialist persona, budget, driver, and permission setup for spawn and resume
resolveSpecialistSetup
  :: Config
  -> AgentMode
  -> ModelDriver
  -> Text
  -> [Text]
  -> [Text]
  -> [Text]
  -> (Text, Int, ModelDriver, SubAgentPermissions)
resolveSpecialistSetup cfg curMode driver role extraPrevAllow extraPrevDeny extraGrants =
  let mSpecialist = Map.lookup (T.toLower role) (specialists cfg)
      specPrompt = case mSpecialist of
        Just s  -> specialistPrompt s
        Nothing -> "# Identity: Specialist SubAgent [" <> role <> "]\nYou are an ephemeral specialist subagent for an isolated task."
      defaultCaps   = maybe [] specialistCapabilities mSpecialist
      defaultDenies = maybe [] specialistDenyGlobs mSpecialist
      defaultBud    = maybe 6 specialistBudget mSpecialist
      combinedGlobs = nubOrd (extraPrevAllow ++ defaultCaps ++ extraGrants)
      combinedDenies = nubOrd (extraPrevDeny ++ defaultDenies)
      isDestructiveGlob g = any (`T.isPrefixOf` g) ["bash*", "write_file*", "edit_file*", "replace_lines*"]
      effectiveGrants = if curMode == PlanMode
        then filter (not . isDestructiveGlob) combinedGlobs
        else combinedGlobs
      grantedWithReport = nubOrd ("submit_report*" : effectiveGrants)
      subPerms = SubAgentPermissions (Set.fromList grantedWithReport) (Set.fromList combinedDenies)
      effectiveDriver = case mSpecialist >>= specialistModel of
        Just rawMod | not (T.null (T.strip rawMod)) -> driverWithModel driver (T.strip rawMod)
        _ -> driver
  in (specPrompt, defaultBud, effectiveDriver, subPerms)

-- | Spawns and executes an ephemeral specialist subagent with isolated STM context and bounded budget
runEphemeralSubAgent
  :: AppEngineState
  -> ModelDriver
  -> SubAgentSpec
  -> IO (Either Text SubAgentReport)
runEphemeralSubAgent engineState driver spec =
  fmap (fmap snd) (runEphemeralSubAgentWithCallId engineState driver Nothing spec)

runEphemeralSubAgentWithCallId
  :: AppEngineState
  -> ModelDriver
  -> Maybe Text
  -> SubAgentSpec
  -> IO (Either Text (Int, SubAgentReport))
runEphemeralSubAgentWithCallId engineState@AppEngineState{..} driver mParentCallId SubAgentSpec{..} = do
  turns <- readTVarIO appTurns
  let parentTurnId = if null turns then Nothing else Just (turnId (last turns))
  curMode <- readTVarIO appMode

  let (specPrompt, defaultBud, effectiveDriver, subPerms) =
        resolveSpecialistSetup appConfig curMode driver subAgentRoleName [] [] subGrantedGlobs
      effectiveBudget = max 1 (if subTurnBudget > 0 then subTurnBudget else defaultBud)

  task <- registerSubAgentTaskFull engineState subAgentRoleName subTaskDescription effectiveBudget parentTurnId mParentCallId
  let sId = subAgentId task
  _ <- updateSubAgentTask engineState sId $ \t -> t { subAgentPermissions = subPerms }

  -- Step 2: Isolated ephemeral state with Depth-1 Star Graph enforcement
  let singleTurnNotice =
        if effectiveBudget == 1
          then "\n\n[CRITICAL NOTICE: SINGLE-TURN BUDGET (1/1). You have exactly ONE turn. You MUST synthesize your answer and call `submit_report` on this turn.]"
          else ""
      rawProjCtx = T.strip (projectContext appConfig)
      subProjCtxSection =
        if T.null rawProjCtx
          then []
          else
            let capped = if T.length rawProjCtx > 6144
                  then T.take 6144 rawProjCtx <> "\n... [project instructions truncated for subagent]\n</project_instructions>"
                  else rawProjCtx
            in ["", capped]
      sysPrompt = T.unlines
        ([ specPrompt
        , ""
        , "## Workspace: " <> T.pack (workspaceRoot appConfig)
        , "## Operating Mode: " <> (if curMode == PlanMode then "[PLAN MODE - Read-Only]" else "[EXEC MODE - Mutating]")
        , "## Turn Budget: " <> T.pack (show effectiveBudget) <> " turns"
        , "## Assigned Objective: " <> subTaskDescription
        , ""
        , "## Reporting Invariant:"
        , "Operate within your turn budget (" <> T.pack (show effectiveBudget) <> " turns maximum). You have access to the synthetic tool `submit_report`."
        , "When your work, survey, diagnosis, profiling, or implementation is complete — or on your final turn — you MUST call `submit_report` with status, summary, and details."
        ] ++ subProjCtxSection) <> singleTurnNotice
      initTurns =
        [ Turn 1 SystemRole [TextBlock sysPrompt]
        , Turn 2 UserRole [TextBlock ("Begin task: " <> subTaskDescription)]
        ]
  subTurnsVar <- newTVarIO initTurns
  updateSubAgentTurns engineState sId initTurns

  -- Step 3: Run execution loop with per-subagent cancellation and artifact capture
  fmap (fmap (\rep -> (sId, rep))) (executeSubAgentSession engineState effectiveDriver sId subPerms subTurnsVar 1 effectiveBudget)

-- | Compacts subagent conversation history when total tokens exceed 85% of the model context limit
-- (capped at 200k tokens to avoid context rot on 1M-window models),
-- preserving the leading specialist SystemRole prompt turn and tool-call pairing.
compactSubAgentTurns :: Int -> [Turn] -> [Turn]
compactSubAgentTurns targetLimit turns =
  let triggerLimit = max 64 (min 200000 ((targetLimit * 85) `div` 100))
  in if estimateTotalTokens turns <= triggerLimit
       then turns
       else case turns of
         (sysTurn@(Turn _ SystemRole _) : rest) ->
           let sysTokens = estimateTotalTokens [sysTurn]
               availBudget = max 64 (triggerLimit - sysTokens)
               compacted4 = compactHistory availBudget 4 rest
               compacted = if estimateTotalTokens compacted4 > availBudget
                 then compactHistory availBudget 2 rest
                 else compacted4
           in sysTurn : compacted
         _ ->
           let compacted4 = compactHistory triggerLimit 4 turns
           in if estimateTotalTokens compacted4 > triggerLimit
                then compactHistory triggerLimit 2 turns
                else compacted4

subAgentLoop
  :: AppEngineState
  -> ModelDriver
  -> Int
  -> SubAgentPermissions
  -> TVar [Turn]
  -> TVar [Text]
  -> TVar (Maybe SubAgentReport)
  -> Int
  -> Int
  -> IO (Either Text SubAgentReport)
subAgentLoop engineState@AppEngineState{..} driver sId subPerms turnsVar attemptedToolsVar reportVar rawCurrentTurn rawBudget
  | currentTurn > budget = do
      mReport <- readTVarIO reportVar
      case mReport of
        Just rep -> pure $ Right rep
        Nothing -> do
          attempted <- readTVarIO attemptedToolsVar
          turns <- readTVarIO turnsVar
          let attemptedSummary = if null attempted
                then "none"
                else T.intercalate ", " (reverse attempted)
              lastNotes = [ t | Turn _ AssistantRole blks <- reverse turns, TextBlock t <- blks, not (T.null (T.strip t)) ]
              notesSuffix = case lastNotes of
                (n:_) -> "\nLast specialist notes: " <> T.take 500 n
                []    -> ""
              failRep = SubAgentReport
                { reportStatus   = "INCONCLUSIVE"
                , reportSummary  = "Turn budget exhausted after " <> T.pack (show budget) <> " turns without submitting a report (use resume_subagent with subagent_id=" <> T.pack (show sId) <> " to continue)."
                , reportDetails  = "Attempted tools: [" <> attemptedSummary <> "]" <> notesSuffix
                , reportArtifact = Nothing
                }
          pure $ Right failRep
  | otherwise = do
      rawTurns <- readTVarIO turnsVar
      subsMap <- readTVarIO appSubAgents
      defaultLimit <- readTVarIO appContextLimit
      let subLimit = case Map.lookup sId subsMap >>= \t -> Map.lookup (T.toLower (subAgentRole t)) (specialists appConfig) >>= specialistModel of
            Just rawMod | not (T.null (T.strip rawMod)) -> lookupModelContextLimitWithConfig appConfig (T.strip rawMod)
            _ -> defaultLimit
          turns = compactSubAgentTurns subLimit rawTurns
      when (turns /= rawTurns) $ do
        atomically $ writeTVar turnsVar turns
        updateSubAgentTurns engineState sId turns

      reg <- readTVarIO appToolRegistry
      mode <- readTVarIO appMode
      let toolSchemas = toolsToOpenAISchema mode reg

      let nextAsstId = if null turns then 1 else maximum (map turnId turns) + 1
          inFlightTurn = Turn nextAsstId AssistantRole [TextBlock "Thinking..."]
      asstTurns0 <- atomically $ do
        modifyTVar' turnsVar (\ts -> ts ++ [inFlightTurn])
        readTVar turnsVar
      updateSubAgentTurns engineState sId asstTurns0

      let updateInFlight blocks = do
            updated <- atomically $ do
              ts <- readTVar turnsVar
              let ts' = map (\t -> if turnId t == nextAsstId then t { turnBlocks = blocks } else t) ts
              writeTVar turnsVar ts'
              pure ts'
            updateSubAgentTurns engineState sId updated

          onSubProgress t th = do
            let blks = [ ThinkingBlock currentTurn th Visible | not (T.null th) ]
                    ++ [ TextBlock t | not (T.null t) ]
            unless (null blks) $ updateInFlight blks
          onSubChunk _ = pure ()

      localIntrVar <- newTVarIO False
      (fullText, fullThinking, toolCalls, wasInterrupted, mDriverErr) <-
        accumulateStreamChunks localIntrVar driver turns toolSchemas onSubProgress onSubChunk

      case mDriverErr of
        Just err -> pure $ Left ("Model Driver Error: " <> err)
        Nothing
          | wasInterrupted -> do
              let intBlocks =
                    [ ThinkingBlock currentTurn fullThinking Visible | not (T.null fullThinking) ]
                    ++ [ TextBlock (fullText <> (if T.null fullText then "" else "\n") <> "⚠️ [Subagent cancelled by user]") ]
              updateInFlight intBlocks
              pure $ Left "Subagent execution cancelled by user."
          | otherwise -> do
              let assistantBlocks =
                    [ ThinkingBlock currentTurn fullThinking Visible | not (T.null fullThinking) ]
                    ++ [ TextBlock fullText | not (T.null fullText) ]
                    ++ map ToolCallBlock toolCalls

              updateInFlight assistantBlocks

              if null toolCalls
                then do
                  mReport <- readTVarIO reportVar
                  case mReport of
                    Just rep -> pure $ Right rep
                    Nothing  ->
                      if any (`T.isInfixOf` fullText) ["[API Error", "[HTTP/Network Exception", "[Configuration Error"]
                        then pure $ Left ("Specialist aborted on API error: " <> T.take 200 fullText)
                        else pure $ Right $ SubAgentReport "COMPLETED" fullText "" Nothing
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

                  subsNow <- readTVarIO appSubAgents
                  let callerDesc = case Map.lookup sId subsNow of
                        Just task -> "[" <> subAgentRole task <> "] " <> subAgentHypothesis task
                        Nothing   -> "SubAgent #" <> T.pack (show sId)

                  -- Execute tool calls concurrently in subagent context
                  rawToolResults <- mapConcurrently (executeToolDispatchWithPerms engineState (SubAgentId sId callerDesc) subPerms) toolCalls
                  mReportNow <- readTVarIO reportVar
                  let finalTurnNotice =
                        if (currentTurn == budget - 1 || budget == 1) && mReportNow == Nothing && not (null rawToolResults)
                          then "\n\n[SYSTEM NOTICE — TURN " <> T.pack (show currentTurn) <> "/" <> T.pack (show budget) <> " (FINAL TURN): Next turn is your last turn. Synthesize your findings now and call `submit_report` immediately.]"
                          else ""
                      toolResults = case reverse rawToolResults of
                        (lastR : prevRs) | not (T.null finalTurnNotice) ->
                          reverse (lastR { resultStdout = resultStdout lastR <> finalTurnNotice } : prevRs)
                        _ -> rawToolResults
                  turnsAfterAsst <- readTVarIO turnsVar
                  let nextToolId = if null turnsAfterAsst then 2 else maximum (map turnId turnsAfterAsst) + 1
                      toolTurn = Turn nextToolId ToolRole (map ToolResultBlock toolResults)
                  toolTurns <- atomically $ do
                    modifyTVar' turnsVar (\ts -> ts ++ [toolTurn])
                    readTVar turnsVar
                  updateSubAgentTurns engineState sId toolTurns

                  mReport <- readTVarIO reportVar
                  case mReport of
                    Just rep -> pure $ Right rep
                    Nothing  -> subAgentLoop engineState driver sId subPerms turnsVar attemptedToolsVar reportVar (currentTurn + 1) budget
  where
    currentTurn = max 1 rawCurrentTurn
    budget = max 1 rawBudget

-- | Shared low-level stream chunk accumulator for orchestrator and subagents
accumulateStreamChunks
  :: TVar Bool
  -> ModelDriver
  -> [Turn]
  -> [Aeson.Value]
  -> (Text -> Text -> IO ())
  -> (StreamChunk -> IO ())
  -> IO (Text, Text, [ToolCall], Bool, Maybe Text)
accumulateStreamChunks intrVar driver turns toolSchemas onProgress onChunk = do
  accumTextVar <- newTVarIO ("" :: Text)
  accumThinkingVar <- newTVarIO ("" :: Text)
  toolCallsVar <- newTVarIO ([] :: [ToolCall])
  driverErrorVar <- newTVarIO (Nothing :: Maybe Text)

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
        ChunkError err -> atomically $
          writeTVar driverErrorVar (Just err)
        ChunkDone -> pure ()

  fullText <- readTVarIO accumTextVar
  fullThinking <- readTVarIO accumThinkingVar
  rawToolCalls <- readTVarIO toolCallsVar
  wasInterrupted <- readTVarIO intrVar
  mDriverErr <- readTVarIO driverErrorVar
  pure (fullText, fullThinking, map finalizeToolArgs rawToolCalls, wasInterrupted, mDriverErr)

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
spawnSpecialistSubAgentTool engineState driver =
  let catalog = formatSpecialistCatalog (specialists (appConfig engineState))
  in ToolDefinition
    { toolName = "spawn_specialist_subagent"
    , toolDescription = "Spawn an ephemeral specialist subagent to execute an isolated task. Subagents run isolated and report via submit_report. Configured specialists: " <> catalog
    , toolParameters = Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object
            [ "role" .= Aeson.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Specialist role to spawn. Available configured specialists: " <> catalog)
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
                , "description" .= ("Optional list of capability glob patterns (e.g. ['read_file*', 'edit_file*', 'sd_*', 'git diff*'])." :: Text)
                ]
            ]
        , "required" .= (["role", "task"] :: [Text])
        ]
    , toolCapability = ReadOnly
    , toolIsMcp = False
    , toolScope = RootOnly
    , toolExecute = \_caller mCallId args -> do
        case parseEither parseSpecialistArgs args of
          Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
          Right (role, task, mBudget, mGrants) -> do
            let budget = fromMaybe 0 mBudget
                grants = fromMaybe [] mGrants
                spec = SubAgentSpec role task budget grants
            res <- runEphemeralSubAgentWithCallId engineState driver mCallId spec
            case res of
              Left err -> pure $ ToolResult "" "" ("Specialist subagent failed: " <> err) Nothing
              Right (sId, rep@SubAgentReport{..}) -> do
                let summary = formatSpecialistReportXml (Just sId) role rep
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
resumeEphemeralSubAgent engineState driver sId addBudget mNewRole mGuidance additionalGrants =
  resumeEphemeralSubAgentWithCallId engineState driver Nothing sId addBudget mNewRole mGuidance additionalGrants

resumeEphemeralSubAgentWithCallId
  :: AppEngineState
  -> ModelDriver
  -> Maybe Text        -- ^ mParentCallId
  -> Int               -- ^ sId
  -> Int               -- ^ additionalBudget
  -> Maybe Text        -- ^ mNewRole
  -> Maybe Text        -- ^ mGuidance
  -> [Text]            -- ^ additionalGrants
  -> IO (Either Text SubAgentReport)
resumeEphemeralSubAgentWithCallId engineState@AppEngineState{..} driver mParentCallId sId addBudget mNewRole mGuidance additionalGrants = do
  prep <- atomically $ do
    subs <- readTVar appSubAgents
    case Map.lookup sId subs of
      Nothing -> pure $ Left $ "SubAgent #" <> T.pack (show sId) <> " not found."
      Just task
        | subAgentStatus task == SubAgentRunning ->
            pure $ Left $ "SubAgent #" <> T.pack (show sId) <> " is already running."
        | otherwise -> do
            curMode <- readTVar appMode
            let currentRole = subAgentRole task
                effectiveRole = case mNewRole of
                  Just r | not (T.null (T.strip r)) -> T.strip r
                  _ -> currentRole
                isSpecialized = T.toLower effectiveRole /= T.toLower currentRole
                prevAllow = if isSpecialized then [] else Set.toList (sapAllow (subAgentPermissions task))
                prevDeny  = Set.toList (sapDeny (subAgentPermissions task))
                (specPrompt, defaultBud, effectiveDriver, subPerms) =
                  resolveSpecialistSetup appConfig curMode driver effectiveRole prevAllow prevDeny additionalGrants
                effectiveAddBudget = max 1 (if addBudget > 0 then addBudget else defaultBud)
                newTotalBudget = max 1 (subAgentBudget task + effectiveAddBudget)
                continuationLines =
                  [ "## Task Resumed"
                  , "## Operating Mode: " <> (if curMode == PlanMode then "[PLAN MODE - Read-Only]" else "[EXEC MODE - Mutating]")
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
                asstCount = length (filter (\t -> turnRole t == AssistantRole) oldTurns)
                startTurn = asstCount + 1
                updatedTask = task
                  { subAgentRole = effectiveRole
                  , subAgentBudget = newTotalBudget
                  , subAgentStatus = SubAgentRunning
                  , subAgentTurns = updatedInitTurns
                  , subAgentPermissions = subPerms
                  , subAgentParentCallId = mParentCallId
                  }
            writeTVar appSubAgents (Map.insert sId updatedTask subs)
            writeTQueue appEventQueue (EvSubAgentUpdate updatedTask)
            pure $ Right (effectiveDriver, subPerms, updatedInitTurns, startTurn, newTotalBudget)
  case prep of
    Left err -> pure (Left err)
    Right (effectiveDriver, subPerms, updatedInitTurns, startTurn, newTotalBudget) -> do
      subTurnsVar <- newTVarIO updatedInitTurns
      executeSubAgentSession engineState effectiveDriver sId subPerms subTurnsVar startTurn newTotalBudget

-- | Tool to resume or specialize an existing subagent
resumeSpecialistSubAgentTool :: AppEngineState -> ModelDriver -> ToolDefinition
resumeSpecialistSubAgentTool engineState driver =
  let catalog = formatSpecialistCatalog (specialists (appConfig engineState))
  in ToolDefinition
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
                , "description" .= ("Optional specialist role to pivot into. Available specialists: " <> catalog)
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
    , toolScope = RootOnly
    , toolExecute = \_caller mCallId args -> do
        case parseEither parseResumeArgs args of
          Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
          Right (sId, mBudget, mGuidance, mRole, mGrants) -> do
            let budget = fromMaybe 0 mBudget
                grants = fromMaybe [] mGrants
            res <- resumeEphemeralSubAgentWithCallId engineState driver mCallId sId budget mRole mGuidance grants
            case res of
              Left err -> pure $ ToolResult "" "" ("Failed to resume subagent #" <> T.pack (show sId) <> ": " <> err) Nothing
              Right rep@SubAgentReport{..} -> do
                subsAfter <- readTVarIO (appSubAgents engineState)
                let actualRole = maybe (fromMaybe "specialist" mRole) subAgentRole (Map.lookup sId subsAfter)
                    summary = formatSpecialistReportXml (Just sId) actualRole rep
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
