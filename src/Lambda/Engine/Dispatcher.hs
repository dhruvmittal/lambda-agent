{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Dispatcher
  ( executeToolDispatch
  ) where

import Control.Concurrent.STM
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath.Glob (compile, match)

import Lambda.Core.ToolProvider
import Lambda.Engine.Security (checkAuthorization)
import Lambda.Engine.State (AppEngineState(..))
import Lambda.Types

-- | Dispatches tool calls with strict mode enforcement and capability security
executeToolDispatch
  :: AppEngineState
  -> CallerContext
  -> [Text]              -- Pre-authorized capability grants (for SubAgents)
  -> ToolCall
  -> IO ToolResult
executeToolDispatch AppEngineState{..} caller grantedGlobs ToolCall{..} = do
  (mode, reg) <- atomically $ do
    m <- readTVar appMode
    r <- readTVar appToolRegistry
    pure (m, r)

  case lookupTool toolCallName reg of
    Nothing ->
      pure $ ToolResult toolCallId "" ("Tool not found in registry: " <> toolCallName) Nothing
    Just ToolDefinition{..} -> do
      -- Step 1: Mode Safety Invariant (/plan vs /exec)
      if mode == PlanMode && toolCapability == Destructive
        then pure $ ToolResult toolCallId ""
               "Execution rejected: Mutating / destructive operations are disabled in [/plan] mode. Switch to [/exec] to run."
               Nothing
        else do
          -- Step 2: Capability & Security Authorization
          authorized <- case caller of
            SubAgentId _ _ -> do
              -- SubAgent capability check against pre-authorized grants
              let cmdStr = case parseEither (Aeson.withObject "args" (Aeson..: "command")) toolCallArgs of
                    Right (cmd :: Text) -> T.unpack cmd
                    _                   -> T.unpack toolCallName
                  matchesGrant = any (\g -> match (compile $ T.unpack g) cmdStr) grantedGlobs
              pure matchesGrant
            MainAgent -> do
              -- Primary agent checks against SecurityState (globs + modal prompt queue)
              let cmdText = case parseEither (Aeson.withObject "args" (Aeson..: "command")) toolCallArgs of
                    Right (cmd :: Text) -> cmd
                    _                   -> toolCallName
              checkAuthorization appSecurity caller cmdText toolCallArgs

          if not authorized
            then pure $ ToolResult toolCallId ""
                   ("Permission Denied: Operation not authorized for " <> T.pack (show caller))
                   Nothing
            else do
              -- Step 3: Execute tool
              res <- toolExecute caller toolCallArgs
              pure res { resultCallIdRef = toolCallId }
