{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Edit as E
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, readTQueue, writeTVar, readTVarIO)
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCross
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import Lambda.Config (loadConfig, Config(..))
import Lambda.Core.EngineInterface (initEngineChannels, startEngineLoop, EngineChannels(..))
import Lambda.Core.ToolProvider (emptyRegistry, registerTools, registerTool)
import Lambda.Driver.OpenAI (openAiDynamicDriver)
import Lambda.Engine.Security (initSecurity)
import Lambda.Engine.Session
  ( Session(..)
  , SessionMeta(..)
  , exportSessionTrace
  , getLatestSession
  , listSessions
  , loadSession
  , renderSessionTraceMarkdown
  )
import Lambda.Engine.State (initEngineStateWithSession, AppEngineState(..))
import Lambda.Engine.SubAgent (spawnSpecialistSubAgentTool)
import Lambda.Provider.Builtin (builtinTools)
import Lambda.Provider.Mcp (startAndLoadMcpServers)
import Lambda.Types
import Lambda.UI.Draw (drawApp)
import Lambda.UI.Events (handleAppEvent)
import Lambda.UI.Types

theApp :: App UIState EngineEvent ResourceName
theApp = App
  { appDraw         = drawApp
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleAppEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr
      [ (attrName "planBadge",      V.black `on` V.cyan)
      , (attrName "execBadge",      V.black `on` V.yellow)
      , (attrName "headerBar",      V.white `on` V.blue)
      , (attrName "titleLogo",      fg V.brightWhite `V.withStyle` V.bold)
      , (attrName "paneTitle",      fg V.brightCyan `V.withStyle` V.bold)
      , (attrName "systemRole",     fg V.cyan)
      , (attrName "userRole",       fg V.brightGreen `V.withStyle` V.bold)
      , (attrName "assistantRole",  fg V.brightMagenta `V.withStyle` V.bold)
      , (attrName "toolRole",       fg V.yellow)
      , (attrName "toolCall",       fg V.yellow)
      , (attrName "toolResult",     fg V.white)
      , (attrName "toolError",      fg V.brightRed)
      , (attrName "diffAdd",        fg V.brightGreen)
      , (attrName "diffRemove",     fg V.brightRed)
      , (attrName "diffHeader",     fg V.brightYellow `V.withStyle` V.bold)
      , (attrName "artifact",       fg V.brightCyan)
      , (attrName "thinkingDim",    fg V.brightBlack)
      , (attrName "subRunning",     fg V.green)
      , (attrName "subSuccess",     fg V.cyan)
      , (attrName "subFailed",      fg V.red)
      , (attrName "subRole",        fg V.brightMagenta `V.withStyle` V.bold)
      , (attrName "selectedBadge",  V.black `on` V.brightYellow `V.withStyle` V.bold)
      , (attrName "ctxNormal",      fg V.brightCyan)
      , (attrName "ctxWarn",        fg V.yellow `V.withStyle` V.bold)
      , (attrName "ctxHigh",        fg V.brightRed `V.withStyle` V.bold)
      , (attrName "modelBadge",     fg V.brightMagenta)
      , (attrName "promptLogo",     fg V.brightWhite `V.withStyle` V.bold)
      , (attrName "promptModel",    fg V.brightMagenta)
      , (attrName "promptDivider",  fg V.brightBlack)
      , (attrName "promptArrow",    fg V.brightCyan `V.withStyle` V.bold)
      , (attrName "promptPlanMode", V.black `on` V.cyan)
      , (attrName "promptExecMode", V.black `on` V.yellow)
      , (attrName "userAccent",     fg V.brightMagenta `V.withStyle` V.bold)
      , (attrName "turnFooter",     fg V.brightBlack)
      , (attrName "hudBorder",      fg V.brightCyan)
      , (attrName "hudTitle",       fg V.brightWhite `V.withStyle` V.bold)
      , (attrName "hudSection",     fg V.brightYellow `V.withStyle` V.bold)
      , (attrName "hudKey",         fg V.cyan)
      , (attrName "compSelected",   V.black `on` V.brightCyan)
      , (attrName "compItem",       fg V.white)
      , (attrName "compBorder",     fg V.brightBlack)
      ]
  }

main :: IO ()
main = do
  -- 1. Load configuration and workspace rules
  cfg <- loadConfig "."
  args <- getArgs
  let sessDir = workspaceRoot cfg </> ".lambda" </> "sessions"

  -- Handle CLI-only commands
  case args of
    ["--list-sessions"] -> do
      metas <- listSessions sessDir
      let total = length metas
          recent = take 20 metas
          formatMeta m =
            let timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (metaUpdatedAt m)
                turnsStr = T.pack $ show (metaTurnCount m)
                agentsStr = T.pack $ show (metaSubAgentCount m)
            in "  " <> metaId m <> " | " <> timeStr <> " | " <> turnsStr <> " turns | " <> agentsStr <> " subagents | " <> metaTitle m
      putStrLn $ "Recent Sessions (" ++ show (length recent) ++ " of " ++ show total ++ "):"
      if null metas
        then putStrLn "  No saved sessions found in .lambda/sessions/"
        else mapM_ (TIO.putStrLn . formatMeta) recent
      exitSuccess
    ["--inspect"] -> do
      mTop <- getLatestSession sessDir
      case mTop of
        Just s  -> TIO.putStrLn (renderSessionTraceMarkdown s) >> exitSuccess
        Nothing -> putStrLn "No saved sessions found to inspect." >> exitFailure
    ["-i"] -> do
      mTop <- getLatestSession sessDir
      case mTop of
        Just s  -> TIO.putStrLn (renderSessionTraceMarkdown s) >> exitSuccess
        Nothing -> putStrLn "No saved sessions found to inspect." >> exitFailure
    ["--inspect", sid] -> do
      loadRes <- loadSession sessDir (T.pack sid)
      case loadRes of
        Left err -> TIO.putStrLn ("Error: " <> err) >> exitFailure
        Right s  -> TIO.putStrLn (renderSessionTraceMarkdown s) >> exitSuccess
    ["-i", sid] -> do
      loadRes <- loadSession sessDir (T.pack sid)
      case loadRes of
        Left err -> TIO.putStrLn ("Error: " <> err) >> exitFailure
        Right s  -> TIO.putStrLn (renderSessionTraceMarkdown s) >> exitSuccess
    ["--export-trace"] -> do
      mTop <- getLatestSession sessDir
      case mTop of
        Just s  -> do
          p <- exportSessionTrace sessDir s
          putStrLn $ "Exported trace to: " ++ p
          exitSuccess
        Nothing -> putStrLn "No saved sessions found to export." >> exitFailure
    ["--export-trace", sid] -> do
      loadRes <- loadSession sessDir (T.pack sid)
      case loadRes of
        Left err -> TIO.putStrLn ("Error: " <> err) >> exitFailure
        Right s  -> do
          p <- exportSessionTrace sessDir s
          putStrLn $ "Exported trace to: " ++ p
          exitSuccess
    _ -> pure ()

  -- Parse session resumption for TUI launch
  loadedSession <- case args of
    ["--continue"] -> do
      mTop <- getLatestSession sessDir
      case mTop of
        Just s -> do
          putStrLn $ "Resuming session: " ++ T.unpack (sessionId s) ++ " (" ++ T.unpack (sessionTitle s) ++ ")"
          pure (Just s)
        Nothing -> do
          putStrLn "No existing session found in .lambda/sessions/. Starting a fresh session."
          pure Nothing
    ["-c"] -> do
      mTop <- getLatestSession sessDir
      case mTop of
        Just s -> do
          putStrLn $ "Resuming session: " ++ T.unpack (sessionId s) ++ " (" ++ T.unpack (sessionTitle s) ++ ")"
          pure (Just s)
        Nothing -> do
          putStrLn "No existing session found in .lambda/sessions/. Starting a fresh session."
          pure Nothing
    ["--session", sid] -> do
      loadRes <- loadSession sessDir (T.pack sid)
      case loadRes of
        Left err -> do
          putStrLn $ "Error resuming session " ++ sid ++ ": " ++ T.unpack err
          exitFailure
        Right s -> do
          putStrLn $ "Resuming session: " ++ sid ++ " (" ++ T.unpack (sessionTitle s) ++ ")"
          pure (Just s)
    ["-s", sid] -> do
      loadRes <- loadSession sessDir (T.pack sid)
      case loadRes of
        Left err -> do
          putStrLn $ "Error resuming session " ++ sid ++ ": " ++ T.unpack err
          exitFailure
        Right s -> do
          putStrLn $ "Resuming session: " ++ sid ++ " (" ++ T.unpack (sessionTitle s) ++ ")"
          pure (Just s)
    [] -> pure Nothing
    _  -> do
      putStrLn "Usage: lambda [--continue|-c] [--session|-s <id>] [--list-sessions] [--inspect [id]] [--export-trace [id]]"
      exitFailure

  -- 2. Initialize tool registry with builtins and configured MCP servers
  let builtinReg = registerTools (builtinTools (workspaceRoot cfg) (artifactDir cfg)) emptyRegistry
  (_mcpClients, mcpTools) <- startAndLoadMcpServers (mcpServers cfg)
  let baseRegistry = registerTools mcpTools builtinReg

  -- 3. Initialize security state
  secState <- initSecurity (alwaysAllowGlobs cfg) (alwaysDenyGlobs cfg)

  -- 4. Initialize engine state and model driver
  engineState <- initEngineStateWithSession cfg baseRegistry secState loadedSession
  driver <- openAiDynamicDriver cfg (readTVarIO (appActiveModel engineState))

  -- 5. Register SubAgent spawning tools (which require engine state & driver)
  let fullRegistry = registerTool (spawnSpecialistSubAgentTool engineState driver) baseRegistry
  atomically $ writeTVar (appToolRegistry engineState) fullRegistry

  -- 6. Setup frontend/engine communication channels sharing appEventQueue
  channels <- initEngineChannels (appEventQueue engineState)
  startEngineLoop engineState driver channels

  -- 7. Setup Brick event channel
  eventChan <- newBChan 256
  _ <- forkIO $ forever $ do
    ev <- atomically $ readTQueue (evQueue channels)
    writeBChan eventChan ev

  -- 8. Setup Vty terminal with bracketed paste enabled (and mouse trap disabled)
  initialVty <- VCross.mkVty V.defaultConfig
  V.setMode (V.outputIface initialVty) V.BracketedPaste True
  V.setMode (V.outputIface initialVty) V.Mouse True

  -- 9. Initialize UI State from loaded session
  let initialTurns = case loadedSession of
        Just s | not (null (sessionTurns s)) -> sessionTurns s
        _ -> [Turn 1 SystemRole [TextBlock "lambdA initialized. Enter a goal or press /help for commands."]]
      initialSubs = case loadedSession of
        Just s  -> sessionSubAgents s
        Nothing -> Map.empty
      initialMode = case loadedSession of
        Just s  -> sessionMode s
        Nothing -> PlanMode
      initialStVec = case loadedSession of
        Just s | Just sv <- Map.lookup "state_vector" (sessionStateVector s) -> sv
        _ -> "GOAL: Awaiting task\nINVARIANTS: []\nACTIVE_HYPOTHESIS: None\nBLOCKED_ON: User"
      initialPrompts = case loadedSession of
        Just s  -> sessionPromptHistory s
        Nothing -> []

  let initialUIState = UIState
        { uiTurns          = initialTurns
        , uiSubAgents      = initialSubs
        , uiCurrentPrompt  = Nothing
        , uiPendingPrompts = Seq.Empty
        , uiMode           = initialMode
        , uiEditor         = E.editor EditorInput (Just 1) ""
        , uiWorkingState   = initialStVec
        , uiChannels       = channels
        , uiLastEscTime    = Nothing
        , uiContextLimit   = contextWindowLimit cfg
        , uiPromptHistory  = initialPrompts
        , uiHistoryIndex   = Nothing
        , uiSavedDraft     = ""
        , uiModelName      = modelName cfg
        , uiThinkingVisible  = True
        , uiSelectedSubAgent = Nothing
        , uiShowHud          = False
        , uiCompletion       = Nothing
        , uiIsGenerating     = False
        , uiConfig           = cfg
        }

  -- 10. Run Brick TUI
  let buildVty = do
        v <- VCross.mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.BracketedPaste True
        V.setMode (V.outputIface v) V.Mouse True
        pure v
  _ <- customMain initialVty buildVty (Just eventChan) theApp initialUIState
  pure ()
