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
  , sessionToMeta
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
  )
import System.FilePath ((</>), (<.>), takeExtension)

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
    , sessionCreatedAt     = now
    , sessionUpdatedAt     = now
    , sessionTitle         = "New Session"
    , sessionMode          = PlanMode
    , sessionTurns         = []
    , sessionSubAgents     = Map.empty
    , sessionStateVector   = Map.empty
    , sessionPromptHistory = []
    }

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

-- | Find and load the most recently updated session
getLatestSession :: FilePath -> IO (Maybe Session)
getLatestSession sessionsDir = do
  metas <- listSessions sessionsDir
  case metas of
    [] -> pure Nothing
    (top:_) -> do
      res <- loadSession sessionsDir (metaId top)
      case res of
        Left _  -> pure Nothing
        Right s -> pure (Just s)

-- | Prune oldest sessions keeping only the N most recent
pruneSessions :: FilePath -> Int -> IO Int
pruneSessions sessionsDir keepCount
  | keepCount <= 0 = pure 0
  | otherwise = do
      exists <- doesDirectoryExist sessionsDir
      if not exists
        then pure 0
        else do
          metas <- listSessions sessionsDir
          let total = length metas
          if total <= keepCount
            then pure 0
            else do
              let toPrune = drop keepCount metas
                  filesToRemove = concatMap (\m ->
                    let base = sessionsDir </> T.unpack (metaId m)
                    in [base <.> "json", base <.> "trace.md"]
                    ) toPrune
              forM_ filesToRemove $ \f -> do
                fExists <- doesFileExist f
                when fExists (removeFile f)
              pure (length toPrune)

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
