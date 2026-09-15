{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Session
  ( Session(..)
  , SessionMeta(..)
  , newSession
  , newSessionId
  , deriveTitle
  , saveSession
  , loadSession
  , listSessions
  , getLatestSession
  , pruneSessions
  , isMeaningfulMeta
  , listMeaningfulSessions
  , cleanEmptySessions
  , resolveSessionId
  , sessionToMeta
  , forkSession
  , renderSessionTraceMarkdown
  , exportSessionTrace
  ) where

import Control.Monad (forM, forM_, void, when)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , removeFile
  , renameFile
  , getModificationTime
  )
import System.FilePath ((</>), (<.>), takeExtension, dropExtension)

import Lambda.Types
  ( AgentMode(..)
  , ContentBlock(..)
  , Role(..)
  , SubAgentStatus(..)
  , SubAgentTask(..)
  , ToolCall(..)
  , ToolResult(..)
  , Turn(..)
  )

-- | Persistent session representation
data Session = Session
  { sessionId           :: !Text
  , sessionParentId     :: !(Maybe Text)
  , sessionCreatedAt     :: !UTCTime
  , sessionUpdatedAt     :: !UTCTime
  , sessionTitle         :: !Text
  , sessionMode          :: !AgentMode
  , sessionTurns         :: ![Turn]
  , sessionSubAgents     :: !(Map Int SubAgentTask)
  , sessionStateVector   :: !(Map Text Text)
  , sessionPromptHistory :: ![Text]
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Session where
  toJSON Session{..} = Aeson.object
    [ "session_id"     .= sessionId
    , "parent_id"      .= sessionParentId
    , "created_at"     .= sessionCreatedAt
    , "updated_at"     .= sessionUpdatedAt
    , "title"          .= sessionTitle
    , "mode"           .= sessionMode
    , "turns"          .= sessionTurns
    , "subagents"      .= sessionSubAgents
    , "state_vector"   .= sessionStateVector
    , "prompt_history" .= sessionPromptHistory
    ]

instance Aeson.FromJSON Session where
  parseJSON = Aeson.withObject "Session" $ \obj -> do
    sessionId           <- obj .: "session_id"
    sessionParentId     <- obj .:? "parent_id"
    sessionCreatedAt    <- obj .: "created_at"
    sessionUpdatedAt    <- obj .: "updated_at"
    sessionTitle        <- obj .: "title"
    sessionMode         <- obj .: "mode"
    sessionTurns        <- obj .: "turns"
    sessionSubAgents    <- obj .:? "subagents" Aeson..!= Map.empty
    sessionStateVector  <- obj .:? "state_vector" Aeson..!= Map.empty
    sessionPromptHistory <- obj .:? "prompt_history" Aeson..!= []
    pure Session{..}

-- | Lightweight session descriptor for listings and selection menus
data SessionMeta = SessionMeta
  { metaId            :: !Text
  , metaParentId      :: !(Maybe Text)
  , metaCreatedAt     :: !UTCTime
  , metaUpdatedAt     :: !UTCTime
  , metaTitle         :: !Text
  , metaTurnCount     :: !Int
  , metaSubAgentCount :: !Int
  , metaMode          :: !AgentMode
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON SessionMeta
instance Aeson.FromJSON SessionMeta

-- | Extract lightweight metadata from a loaded Session
sessionToMeta :: Session -> SessionMeta
sessionToMeta Session{..} = SessionMeta
  { metaId            = sessionId
  , metaParentId      = sessionParentId
  , metaCreatedAt     = sessionCreatedAt
  , metaUpdatedAt     = sessionUpdatedAt
  , metaTitle         = sessionTitle
  , metaTurnCount     = length sessionTurns
  , metaSubAgentCount = Map.size sessionSubAgents
  , metaMode          = sessionMode
  }

-- | Generate a unique timestamped session ID
newSessionId :: IO Text
newSessionId = do
  now <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
      picoStr = take 4 (formatTime defaultTimeLocale "%q" now)
  pure $ T.pack $ "session_" ++ timeStr ++ "_" ++ picoStr

-- | Create a fresh blank session with unique ID
newSession :: IO Session
newSession = do
  now <- getCurrentTime
  sid <- newSessionId
  pure Session
    { sessionId           = sid
    , sessionParentId     = Nothing
    , sessionCreatedAt     = now
    , sessionUpdatedAt     = now
    , sessionTitle         = "New Session"
    , sessionMode          = PlanMode
    , sessionTurns         = []
    , sessionSubAgents     = Map.empty
    , sessionStateVector   = Map.empty
    , sessionPromptHistory = []
    }

-- | Forks an existing session with its current turns into a new child session
forkSession :: FilePath -> Session -> Maybe Text -> IO Session
forkSession sessionsDir parentSess mTitle = do
  newSid <- newSessionId
  now <- getCurrentTime
  let childTitle = case mTitle of
        Just t | not (T.null (T.strip t)) -> T.strip t
        _                                 -> sessionTitle parentSess <> " (fork)"
      childSess = parentSess
        { sessionId           = newSid
        , sessionParentId     = Just (sessionId parentSess)
        , sessionCreatedAt     = now
        , sessionUpdatedAt     = now
        , sessionTitle         = childTitle
        }
  saveSession sessionsDir 0 childSess
  pure childSess

-- | Derive a clean title from the first prompt (truncated to 60 chars)
deriveTitle :: Text -> Text
deriveTitle prompt =
  let cleaned = T.strip $ T.takeWhile (/= '\n') prompt
  in if T.null cleaned
       then "Untitled Session"
       else if T.length cleaned > 60
              then T.take 57 cleaned <> "..."
              else cleaned

-- | Atomically save session to .lambda/sessions/<id>.json
saveSession :: FilePath -> Int -> Session -> IO ()
saveSession sessionsDir maxSaved s = do
  createDirectoryIfMissing True sessionsDir
  let sid = sessionId s
      destPath = sessionsDir </> (T.unpack sid ++ ".json")
      tmpPath  = destPath <.> "tmp"
  
  let bytes = Aeson.encode s
  BL.writeFile tmpPath bytes
  renameFile tmpPath destPath

  when (maxSaved > 0) $
    void $ pruneSessions sessionsDir maxSaved

-- | Load a specific session by ID
loadSession :: FilePath -> Text -> IO (Either Text Session)
loadSession sessionsDir sid = do
  let path = sessionsDir </> (T.unpack sid ++ ".json")
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Session file not found: " <> T.pack path
    else do
      content <- BL.readFile path
      case Aeson.eitherDecode content of
        Left err -> pure $ Left $ "Failed to parse session JSON: " <> T.pack err
        Right s  -> pure $ Right s

-- | List all sessions ordered by updated_at descending
listSessions :: FilePath -> IO [SessionMeta]
listSessions sessionsDir = do
  exists <- doesDirectoryExist sessionsDir
  if not exists
    then pure []
    else do
      entries <- listDirectory sessionsDir
      let jsonFiles = filter (\f -> takeExtension f == ".json" && not (".tmp" `isSuffixOf` f)) entries
      metas <- fmap catMaybes $ forM jsonFiles $ \f -> do
        let path = sessionsDir </> f
        content <- BL.readFile path
        case Aeson.eitherDecode content of
          Right (s :: Session) -> pure $ Just (sessionToMeta s)
          Left _               -> pure Nothing
      pure $ sortOn (Down . metaUpdatedAt) metas

-- | Find and load the most recently updated session (prefers sessions with user interaction over empty stubs)
getLatestSession :: FilePath -> IO (Maybe Session)
getLatestSession sessionsDir = do
  exists <- doesDirectoryExist sessionsDir
  if not exists then pure Nothing else do
    entries <- listDirectory sessionsDir
    let jsonFiles = filter (\f -> takeExtension f == ".json" && not (".tmp" `isSuffixOf` f)) entries
    if null jsonFiles then pure Nothing else do
      filesWithTime <- forM jsonFiles $ \f -> do
        let path = sessionsDir </> f
        mtime <- getModificationTime path
        pure (mtime, f)
      let sortedFiles = sortOn (Down . fst) filesWithTime
      -- Find the newest session that has meaningful turns (> 1 turn or user turns)
      let loadCandidate f = do
            let sid = T.pack (dropExtension f)
            res <- loadSession sessionsDir sid
            case res of
              Right s -> pure (Just s)
              Left _  -> pure Nothing
      allLoaded <- catMaybes <$> mapM (loadCandidate . snd) sortedFiles
      let isMeaningful s = length (sessionTurns s) > 1 || any (\t -> turnRole t == UserRole) (sessionTurns s)
      case filter isMeaningful allLoaded of
        (s:_) -> pure (Just s)
        []    -> case allLoaded of
                   (s:_) -> pure (Just s)
                   []    -> pure Nothing

-- | Prune oldest sessions keeping only the N most recent
pruneSessions :: FilePath -> Int -> IO Int
pruneSessions sessionsDir keepCount
  | keepCount <= 0 = pure 0
  | otherwise = do
      exists <- doesDirectoryExist sessionsDir
      if not exists
        then pure 0
        else do
          entries <- listDirectory sessionsDir
          let jsonFiles = filter (\f -> takeExtension f == ".json" && not (".tmp" `isSuffixOf` f)) entries
          let total = length jsonFiles
          if total <= keepCount
            then pure 0
            else do
              filesWithTime <- forM jsonFiles $ \f -> do
                let path = sessionsDir </> f
                mtime <- getModificationTime path
                pure (mtime, f)
              let sortedFiles = map snd (sortOn (Down . fst) filesWithTime)
                  toPrune = drop keepCount sortedFiles
                  filesToRemove = concatMap (\f -> 
                    let base = sessionsDir </> dropExtension f
                    in [base <.> "json", base <.> "trace.md"]
                    ) toPrune
              forM_ filesToRemove $ \f -> do
                fExists <- doesFileExist f
                when fExists (removeFile f)
              pure (length toPrune)

-- | Predicate identifying meaningful sessions that contain actual user interactions or subagent activity.
-- Pristine stub sessions have <= 1 turn, 0 subagents, and title "New Session".
isMeaningfulMeta :: SessionMeta -> Bool
isMeaningfulMeta m =
  metaTurnCount m > 1 || metaSubAgentCount m > 0 || (metaTitle m /= "New Session" && metaTurnCount m > 0)

-- | List meaningful sessions ordered by updated_at descending
listMeaningfulSessions :: FilePath -> IO [SessionMeta]
listMeaningfulSessions sessionsDir = do
  metas <- listSessions sessionsDir
  pure $ filter isMeaningfulMeta metas

-- | Clean up empty 0-interaction stub sessions from .lambda/sessions/
cleanEmptySessions :: FilePath -> IO Int
cleanEmptySessions sessionsDir = do
  exists <- doesDirectoryExist sessionsDir
  if not exists then pure 0 else do
    entries <- listDirectory sessionsDir
    let jsonFiles = filter (\f -> takeExtension f == ".json" && not (".tmp" `isSuffixOf` f)) entries
    deleted <- forM jsonFiles $ \f -> do
      let path = sessionsDir </> f
      content <- BL.readFile path
      case Aeson.eitherDecode content of
        Right (s :: Session) ->
          let isStub = length (sessionTurns s) <= 1
                     && Map.null (sessionSubAgents s)
                     && sessionTitle s == "New Session"
                     && not (any (\t -> turnRole t == UserRole) (sessionTurns s))
          in if isStub
               then do
                 removeFile path
                 let tracePath = sessionsDir </> (dropExtension f <.> "trace.md")
                 tExists <- doesFileExist tracePath
                 when tExists (removeFile tracePath)
                 pure 1
               else pure 0
        Left _ -> pure 0
    pure (sum deleted)

-- | Resolve a session target into a canonical session ID. Target can be:
-- 1. A 1-based index (e.g. "1", "2") corresponding to the N-th most recent meaningful session
-- 2. An exact session ID (e.g. "session_20260912_161506_8714")
-- 3. A case-insensitive substring matching a session title or ID
resolveSessionId :: FilePath -> Text -> IO (Either Text Text)
resolveSessionId sessionsDir rawTarget = do
  let target = T.strip rawTarget
  if T.null target
    then pure $ Left "No session target specified."
    else do
      meaningful <- listMeaningfulSessions sessionsDir
      -- 1. Try 1-based numeric index against meaningful sessions
      case reads (T.unpack target) of
        [(idx, "")] | idx >= 1 && idx <= length meaningful ->
          pure $ Right (metaId (meaningful !! (idx - 1)))
        [(idx, "")] | idx < 1 || idx > length meaningful ->
          pure $ Left $ "Session index " <> target <> " out of range (1.." <> T.pack (show (length meaningful)) <> ")."
        _ -> do
          -- 2. Check exact session ID match (in meaningful or all sessions)
          allMetas <- listSessions sessionsDir
          case filter (\m -> metaId m == target) allMetas of
            (m:_) -> pure $ Right (metaId m)
            [] -> do
              -- 3. Case-insensitive substring match on meaningful session titles
              let lowerTarget = T.toLower target
                  titleMatches = filter (\m -> lowerTarget `T.isInfixOf` T.toLower (metaTitle m)) meaningful
              case titleMatches of
                (m:_) -> pure $ Right (metaId m)
                [] -> do
                  -- 4. Case-insensitive substring match on session ID
                  let idMatches = filter (\m -> lowerTarget `T.isInfixOf` T.toLower (metaId m)) allMetas
                  case idMatches of
                    (m:_) -> pure $ Right (metaId m)
                    [] -> pure $ Left $ "No session found matching '" <> target <> "'."

-- | Render human-readable markdown trace on demand for debugging
renderSessionTraceMarkdown :: Session -> Text
renderSessionTraceMarkdown Session{..} = T.unlines $
  [ "# Session Execution Trace: " <> sessionId
  , ""
  , "- **Title**: " <> sessionTitle
  , "- **Created**: " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" sessionCreatedAt)
  , "- **Updated**: " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" sessionUpdatedAt)
  , "- **Mode**: " <> (case sessionMode of PlanMode -> "PLAN (Read-Only)"; ExecMode -> "EXEC (Mutating)")
  , "- **Primary Turns**: " <> T.pack (show (length sessionTurns))
  , "- **Specialist SubAgents**: " <> T.pack (show (Map.size sessionSubAgents))
  , ""
  , "---"
  , ""
  , "## 1. Primary Dialogue & Orchestration Flow"
  , ""
  ] ++ concatMap renderTurnTrace sessionTurns
    ++
  [ ""
  , "---"
  , ""
  , "## 2. Specialist SubAgents Telemetry"
  , ""
  ] ++ (if Map.null sessionSubAgents
          then ["*No specialist subagents were spawned in this session.*"]
          else concatMap (uncurry renderSubAgentTrace) (Map.toList sessionSubAgents))

renderTurnTrace :: Turn -> [Text]
renderTurnTrace Turn{..} =
  let roleBadge = case turnRole of
        UserRole      -> "USER"
        AssistantRole -> "ASSISTANT (Lead Architect)"
        ToolRole      -> "TOOL OUTPUT"
        SystemRole    -> "SYSTEM"
  in [ "### Turn " <> T.pack (show turnId) <> " [" <> roleBadge <> "]"
     , ""
     ] ++ concatMap renderBlockTrace turnBlocks ++ [""]

renderBlockTrace :: ContentBlock -> [Text]
renderBlockTrace = \case
  TextBlock t ->
    [ t, "" ]
  ThinkingBlock{..} ->
    [ "> **[Thinking / Chain-of-Thought]**"
    , "> " <> T.replace "\n" "\n> " thinkingBody
    , ""
    ]
  ToolCallBlock ToolCall{..} ->
    [ "```bash"
    , "# Tool Invocation: " <> toolCallName <> " (id: " <> toolCallId <> ")"
    , T.pack (show toolCallArgs)
    , "```"
    , ""
    ]
  ToolResultBlock ToolResult{..} ->
    [ "```"
    , "# Result for: " <> resultCallIdRef
    , if T.null resultStdout then "(empty stdout)" else resultStdout
    , if T.null resultStderr then "" else "STDERR:\n" <> resultStderr
    , "```"
    , ""
    ]

renderSubAgentTrace :: Int -> SubAgentTask -> [Text]
renderSubAgentTrace sid SubAgentTask{..} =
  let statusStr = case subAgentStatus of
        SubAgentRunning   -> "RUNNING"
        SubAgentSuccess s -> "SUCCESS: " <> s
        SubAgentBlocked b -> "BLOCKED: " <> b
        SubAgentFailed f  -> "FAILED: " <> f
      artStr = case subAgentArtifact of
        Just p  -> T.pack p
        Nothing -> "None"
  in [ "### SubAgent #" <> T.pack (show sid) <> ": [" <> subAgentRole <> "]"
     , "- **Hypothesis**: " <> subAgentHypothesis
     , "- **Status**: `" <> statusStr <> "`"
     , "- **Turn Budget**: " <> T.pack (show subAgentTurnCount) <> " / " <> T.pack (show subAgentBudget) <> " turns used"
     , "- **Artifact**: " <> artStr
     , ""
     , "#### Internal Reasoning & Tool Runs (" <> T.pack (show (length subAgentTurns)) <> " turns):"
     , ""
     ] ++ concatMap renderTurnTrace subAgentTurns ++ [""]

-- | Export on-demand markdown trace file <id>.trace.md in sessions directory
exportSessionTrace :: FilePath -> Session -> IO FilePath
exportSessionTrace sessionsDir s = do
  createDirectoryIfMissing True sessionsDir
  let sid = sessionId s
      tracePath = sessionsDir </> (T.unpack sid ++ ".trace.md")
      md = renderSessionTraceMarkdown s
  TIO.writeFile tracePath md
  pure tracePath
