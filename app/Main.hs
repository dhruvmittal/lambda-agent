{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Edit as E
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, readTQueue, writeTVar)
import Control.Monad (forever)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCross

import Lambda.Config (loadConfig, Config(..))
import Lambda.Core.EngineInterface (initEngineChannels, startEngineLoop, EngineChannels(..))
import Lambda.Core.ToolProvider (emptyRegistry, registerTools, registerTool)
import Lambda.Driver.OpenAI (openAiDriver)
import Lambda.Engine.Security (initSecurity)
import Lambda.Engine.State (initEngineState, AppEngineState(..))
import Lambda.Engine.SubAgent (spawnSubAgentTool)
import Lambda.Provider.Builtin (builtinTools)
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
      , (attrName "artifact",       fg V.brightCyan)
      , (attrName "thinkingDim",    fg V.brightBlack)
      , (attrName "subRunning",     fg V.green)
      , (attrName "subSuccess",     fg V.cyan)
      , (attrName "subFailed",      fg V.red)
      ]
  }

main :: IO ()
main = do
  -- 1. Load configuration and workspace rules
  cfg <- loadConfig "."

  -- 2. Initialize tool registry with builtins
  let baseRegistry = registerTools (builtinTools (workspaceRoot cfg) (artifactDir cfg)) emptyRegistry

  -- 3. Initialize security state
  secState <- initSecurity (alwaysAllowGlobs cfg) (alwaysDenyGlobs cfg)

  -- 4. Initialize engine state and model driver
  engineState <- initEngineState cfg baseRegistry secState
  driver <- openAiDriver cfg

  -- 5. Register SubAgent spawning tool (which requires engine state & driver)
  let fullRegistry = registerTool (spawnSubAgentTool engineState driver) baseRegistry
  atomically $ writeTVar (appToolRegistry engineState) fullRegistry

  -- 6. Setup frontend/engine communication channels sharing appEventQueue
  channels <- initEngineChannels (appEventQueue engineState)
  startEngineLoop engineState driver channels

  -- 7. Setup Brick event channel
  eventChan <- newBChan 256
  _ <- forkIO $ forever $ do
    ev <- atomically $ readTQueue (evQueue channels)
    writeBChan eventChan ev

  -- 8. Setup Vty terminal with mouse mode enabled
  initialVty <- VCross.mkVty V.defaultConfig
  V.setMode (V.outputIface initialVty) V.Mouse True

  -- 9. Initialize UI State
  let initialUIState = UIState
        { uiTurns          =
            [ Turn 1 SystemRole [TextBlock "lambdA initialized. Enter a goal or press /help for commands."]
            ]
        , uiSubAgents      = Map.empty
        , uiCurrentPrompt  = Nothing
        , uiPendingPrompts = Seq.Empty
        , uiMode           = PlanMode
        , uiEditor         = E.editor EditorInput (Just 1) ""
        , uiWorkingState   = "GOAL: Awaiting task\nINVARIANTS: []\nACTIVE_HYPOTHESIS: None\nBLOCKED_ON: User"
        , uiChannels       = channels
        }

  -- 10. Run Brick TUI
  let buildVty = do
        v <- VCross.mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v
  _ <- customMain initialVty buildVty (Just eventChan) theApp initialUIState
  pure ()
