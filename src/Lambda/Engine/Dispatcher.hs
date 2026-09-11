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

import Control.Exception (SomeException, try)
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
                  cmdText = T.pack cmdStr
                  matchesGrant = any (`matchesPattern` cmdStr) grantedGlobs
              if matchesGrant
                then pure True
                else checkAuthorization appSecurity caller cmdText toolCallArgs
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
              -- Step 3: Execute tool with exception guard
              execRes <- try (toolExecute caller toolCallArgs)
              case execRes of
                Left (ex :: SomeException) ->
                  pure $ ToolResult toolCallId "" ("Tool execution failure: " <> T.pack (show ex)) Nothing
                Right res ->
                  pure res { resultCallIdRef = toolCallId }
  where
    matchesPattern globPat str =
      let patStr = T.unpack globPat
          starPat = if "*" `T.isSuffixOf` globPat && not ("**" `T.isSuffixOf` globPat)
                      then T.unpack (globPat <> "*")
                      else patStr
      in match (compile patStr) str
         || match (compile starPat) str
         || (if "*" `T.isSuffixOf` globPat
               then T.dropEnd 1 globPat `T.isPrefixOf` T.pack str
               else globPat == T.pack str)
