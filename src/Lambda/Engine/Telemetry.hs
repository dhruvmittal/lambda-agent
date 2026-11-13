{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Telemetry
  ( TelemetryHandle
  , startTelemetryWatcher
  , stopTelemetryWatcher
  , sanitizeHerdrMessage
  , formatOscTitle
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try, finally)
import Control.Monad (forever, when)
import Data.Char (isControl, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), (<.>), takeFileName)
import System.IO (hFlush, hIsTerminalDevice, stdout)
import System.Process (createProcess, proc, CreateProcess(..), waitForProcess, interruptProcessGroupOf)
import System.Timeout (timeout)

import Lambda.Config (Config(..))
import Lambda.Engine.Session (Session(..))
import Lambda.Engine.State (AppEngineState(..), readTelemetrySnapshot)
import Lambda.Types

data TelemetryHandle = TelemetryHandle
  { thWatcherAsync :: !(Async ())
  , thWorkerAsync  :: !(Async ())
  , thReleaseAct   :: !(IO ())
  }

-- | Strips control characters (including newlines and escapes), compacts whitespace,
-- and bounds length to 120 chars.
sanitizeHerdrMessage :: Text -> Text
sanitizeHerdrMessage raw =
  let normalizeSpace c
        | isSpace c = ' '
        | otherwise = c
      filtered = T.filter (\c -> not (isControl c) && c /= '\ESC') (T.map normalizeSpace raw)
      clean = T.unwords (T.words filtered)
  in if T.length clean > 120
       then T.take 119 clean <> "…"
       else clean

-- | Formats an OSC 2 title string for the given state and session title
formatOscTitle :: LifecycleState -> Text -> Text
formatOscTitle st title =
  let cleanTitle = sanitizeHerdrMessage title
      titleSuffix = if T.null cleanTitle then "" else " · " <> cleanTitle
  in case st of
       LifecycleIdle -> if T.null cleanTitle then "λ (idle)" else "λ" <> titleSuffix
       LifecycleWorking -> "⠋ lambdA" <> titleSuffix
       LifecycleBlocked reason -> "▲ lambdA [blocked: " <> sanitizeHerdrMessage reason <> "]" <> titleSuffix

-- | Starts the unified, non-blocking telemetry watcher and single-worker conflation dispatcher
startTelemetryWatcher :: AppEngineState -> IO TelemetryHandle
startTelemetryWatcher engineState@AppEngineState{..} = do
  mHerdrEnv   <- lookupEnv "HERDR_ENV"
  mHerdrPane  <- lookupEnv "HERDR_PANE_ID"
  mHerdrBin   <- lookupEnv "HERDR_BIN_PATH"
  isTty       <- hIsTerminalDevice stdout
  execPath    <- getExecutablePath

  let herdrActive = mHerdrEnv == Just "1" && mHerdrPane /= Nothing
      herdrPaneId = maybe "" id mHerdrPane
      herdrBinCmd = maybe "herdr" id mHerdrBin
      execName    = case takeFileName execPath of
        "" -> "lambda"
        nm -> nm

  -- 1-slot mailbox for lossy conflation (ensures at most 1 process in flight)
  mailboxSlot <- newTVarIO (Nothing :: Maybe TelemetrySnapshot)
  seqCounter  <- newTVarIO (1 :: Word64)

  -- Worker thread: consumes the latest snapshot and executes Herdr CLI / OSC 2
  workerAsync <- async $ forever $ do
    snap <- atomically $ do
      mSnap <- readTVar mailboxSlot
      case mSnap of
        Nothing -> retry
        Just s  -> do
          writeTVar mailboxSlot Nothing
          pure s

    -- 1. Native OSC 2 Terminal Title (only if terminal device and not ACP stdout pipe)
    when isTty $ do
      let oscStr = "\ESC]2;" <> formatOscTitle (tsState snap) (tsSessionTitle snap) <> "\BEL"
      _ <- try @SomeException (TIO.hPutStr stdout oscStr >> hFlush stdout)
      pure ()

    -- 2. Herdr CLI reporting (single execution with lossy conflation and process group timeout)
    when herdrActive $ do
      seqVal <- atomically $ do
        n <- readTVar seqCounter
        writeTVar seqCounter (n + 1)
        pure n

      -- Check if session is persisted on disk before advertising resumption
      mPersistedSid <- case tsSessionId snap of
        Just sid -> do
          let sessPath = appSessionDir </> T.unpack sid <.> "json"
          exists <- doesFileExist sessPath
          pure (if exists then Just sid else Nothing)
        Nothing -> pure Nothing

      let (stateFlag, msgArgs) = case tsState snap of
            LifecycleIdle           -> ("idle", [])
            LifecycleWorking        -> ("working", [])
            LifecycleBlocked reason -> ("blocked", ["--message", T.unpack (sanitizeHerdrMessage reason)])

          resumeArgs = case mPersistedSid of
            Just sid -> ["--agent-session-id", T.unpack sid, "--", execName, "--session", T.unpack sid]
            Nothing  -> []

          reportArgs =
            [ "pane", "report-agent", herdrPaneId
            , "--source", "lambda"
            , "--agent", "lambda"
            , "--state", stateFlag
            , "--seq", show seqVal
            ] ++ msgArgs ++ resumeArgs

      _ <- runWithTimeout herdrBinCmd reportArgs 2000000 -- 2 second timeout
      pure ()

  -- Watcher thread: observes pure STM changes in TelemetrySnapshot
  lastSeenVar <- newTVarIO (Nothing :: Maybe TelemetrySnapshot)
  watcherAsync <- async $ forever $ do
    nextSnap <- atomically $ do
      cur <- readTelemetrySnapshot engineState
      prev <- readTVar lastSeenVar
      when (Just cur == prev) retry
      writeTVar lastSeenVar (Just cur)
      -- Update mailbox with lossy conflation (overwrites older pending snapshot)
      writeTVar mailboxSlot (Just cur)
      pure cur
    pure ()

  -- Release action for clean shutdown
  let releaseAction = when herdrActive $ do
        seqVal <- atomically $ do
          n <- readTVar seqCounter
          writeTVar seqCounter (n + 1)
          pure n
        let releaseArgs =
              [ "pane", "release-agent", herdrPaneId
              , "--source", "lambda"
              , "--agent", "lambda"
              , "--seq", show seqVal
              ]
        _ <- runWithTimeout herdrBinCmd releaseArgs 1000000 -- 1 second timeout
        pure ()

  pure TelemetryHandle
    { thWatcherAsync = watcherAsync
    , thWorkerAsync  = workerAsync
    , thReleaseAct   = releaseAction
    }

-- | Shuts down the telemetry watcher, cancelling worker threads and triggering release-agent
stopTelemetryWatcher :: TelemetryHandle -> IO ()
stopTelemetryWatcher TelemetryHandle{..} = do
  cancel thWatcherAsync
  cancel thWorkerAsync
  _ <- try @SomeException thReleaseAct
  pure ()

-- | Spawns a child process with a dedicated process group and terminates the group on timeout
runWithTimeout :: FilePath -> [String] -> Int -> IO ()
runWithTimeout bin args micros = do
  res <- try @SomeException $ do
    let cp = (proc bin args) { create_group = True }
    (_, _, _, ph) <- createProcess cp
    mExit <- timeout micros (waitForProcess ph)
    case mExit of
      Just _  -> pure ()
      Nothing -> do
        -- Explicitly kill process group on timeout to prevent orphan zombies
        _ <- try @SomeException (interruptProcessGroupOf ph)
        _ <- waitForProcess ph
        pure ()
  pure ()
