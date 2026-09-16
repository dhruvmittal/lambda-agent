{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Security
  ( SecurityState(..)
  , initSecurity
  , initSecurityWithRoot
  , deriveSecurityGlob
  , checkAuthorization
  , requestGrantApproval
  , resolvePrompt
  ) where

import Control.Concurrent.STM
import qualified Data.Aeson as Aeson
import Data.List (isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath.Glob (Pattern, compile, match)
import Lambda.Config (persistAllowGlob)
import Lambda.Types

data SecurityState = SecurityState
  { securityWorkspaceRoot :: !FilePath
  , alwaysAllowedPatterns :: !(TVar [String])
  , alwaysDeniedPatterns  :: ![String]
  , alwaysAllowedCompiled :: !(TVar [Pattern])
  , alwaysDeniedCompiled  :: ![Pattern]
  , sessionAllowList      :: !(TVar (Set Text))
  , sessionDenyList       :: !(TVar (Set Text))
  , uiPromptQueue         :: !(TBQueue PermissionPrompt)
  , promptCounter         :: !(TVar Int)
  }

-- | Compile a security glob, supporting both single-level wildcard and recursive slash-traversal
compileSecurityGlob :: String -> [Pattern]
compileSecurityGlob patStr =
  let p1 = compile patStr
      p2 = if "*" `isSuffixOf` patStr && not ("/**" `isSuffixOf` patStr)
             then [compile (patStr ++ "/**"), compile (patStr ++ "*")]
             else []
  in p1 : p2

-- | Derive a clean, predictable glob pattern from a command or tool call
deriveSecurityGlob :: Text -> String
deriveSecurityGlob raw =
  let t = T.strip raw
  in case T.words t of
    [] -> "*"
    [w] ->
      if "*" `T.isSuffixOf` w
        then T.unpack w
        else T.unpack w ++ "*"
    (w1:w2:_)
      | w1 `elem` ["git", "cabal", "cargo", "npm", "yarn", "pnpm", "docker", "kubectl", "gh", "nix"] ->
          T.unpack w1 ++ " " ++ T.unpack w2 ++ "*"
      | w1 == "outside_workspace:" ->
          "outside_workspace: " ++ T.unpack w2 ++ "*"
      | otherwise ->
          T.unpack w1 ++ "*"

initSecurityWithRoot :: FilePath -> [String] -> [String] -> IO SecurityState
initSecurityWithRoot wsRoot allowList denyList = do
  sAllow <- newTVarIO Set.empty
  sDeny  <- newTVarIO Set.empty
  queue  <- newTBQueueIO 64
  pCount <- newTVarIO 1
  let cAllow = concatMap compileSecurityGlob allowList
      cDeny  = concatMap compileSecurityGlob denyList
  tAllowPats <- newTVarIO allowList
  tAllowComp <- newTVarIO cAllow
  pure SecurityState
    { securityWorkspaceRoot = wsRoot
    , alwaysAllowedPatterns = tAllowPats
    , alwaysDeniedPatterns  = denyList
    , alwaysAllowedCompiled = tAllowComp
    , alwaysDeniedCompiled  = cDeny
    , sessionAllowList      = sAllow
    , sessionDenyList       = sDeny
    , uiPromptQueue         = queue
    , promptCounter         = pCount
    }

initSecurity :: [String] -> [String] -> IO SecurityState
initSecurity = initSecurityWithRoot "."

-- | Pragmatic UX safeguard check with non-blocking STM queueing
checkAuthorization :: SecurityState -> CallerContext -> Text -> Aeson.Value -> IO Bool
checkAuthorization SecurityState{..} caller cmd payload = do
  let cmdStr = T.unpack cmd

  -- Step 1: Static Deny List (Deny-first rule)
  if any (`match` cmdStr) alwaysDeniedCompiled
    then pure False
    else do
      -- Step 2: Static & Dynamic Allow List (repetitive safe operations)
      cAllow <- readTVarIO alwaysAllowedCompiled
      if any (`match` cmdStr) cAllow
        then pure True
        else do
          -- Step 3: Session Dynamic Memory
          (isAllowed, isDenied) <- atomically $ do
            a <- Set.member cmd <$> readTVar sessionAllowList
            d <- Set.member cmd <$> readTVar sessionDenyList
            pure (a, d)
          if isAllowed
            then pure True
            else if isDenied
              then pure False
              else do
                -- Step 4: Bubble interactive prompt to TUI
                let proposedGlob = T.pack (deriveSecurityGlob cmd)
                (_reqId, replyVar) <- atomically $ do
                  rId <- readTVar promptCounter
                  writeTVar promptCounter (rId + 1)
                  reply <- newEmptyTMVar
                  let req = PermissionPrompt rId caller cmd payload proposedGlob reply
                  writeTBQueue uiPromptQueue req
                  pure (rId, reply)

                choice <- atomically $ takeTMVar replyVar
                case choice of
                  PermAlways -> do
                    let globStr = T.unpack proposedGlob
                    _ <- persistAllowGlob securityWorkspaceRoot globStr
                    atomically $ do
                      modifyTVar' alwaysAllowedPatterns (++ [globStr])
                      modifyTVar' alwaysAllowedCompiled (++ compileSecurityGlob globStr)
                      modifyTVar' sessionAllowList (Set.insert cmd)
                    pure True
                  PermSession -> do
                    atomically $ modifyTVar' sessionAllowList (Set.insert cmd)
                    pure True
                  PermOnce -> pure True
                  PermDeny -> pure False
                  PermNo   -> pure False
                  PermNever -> do
                    atomically $ modifyTVar' sessionDenyList (Set.insert cmd)
                    pure False

-- | Upfront authorization prompt when spawning a subagent with requested capabilities
requestGrantApproval :: SecurityState -> Int -> Text -> [Text] -> IO Bool
requestGrantApproval SecurityState{..} sId hypothesis requestedGlobs = do
  let grantDesc    = "SubAgent #" <> T.pack (show sId) <> " capability grant"
      payload      = Aeson.object
        [ "subagent_id"  Aeson..= sId
        , "hypothesis"   Aeson..= hypothesis
        , "capabilities" Aeson..= requestedGlobs
        ]
      proposedGlob = T.intercalate ", " requestedGlobs

  (_reqId, replyVar) <- atomically $ do
    rId <- readTVar promptCounter
    writeTVar promptCounter (rId + 1)
    reply <- newEmptyTMVar
    let req = PermissionPrompt rId (SubAgentId sId hypothesis) grantDesc payload proposedGlob reply
    writeTBQueue uiPromptQueue req
    pure (rId, reply)

  choice <- atomically $ takeTMVar replyVar
  case choice of
    PermAlways -> do
      mapM_ (persistAllowGlob securityWorkspaceRoot . T.unpack) requestedGlobs
      atomically $ do
        modifyTVar' alwaysAllowedPatterns (++ map T.unpack requestedGlobs)
        modifyTVar' alwaysAllowedCompiled (++ concatMap (compileSecurityGlob . T.unpack) requestedGlobs)
        modifyTVar' sessionAllowList (\s -> foldr Set.insert s requestedGlobs)
      pure True
    PermSession -> do
      atomically $ modifyTVar' sessionAllowList (\s -> foldr Set.insert s requestedGlobs)
      pure True
    PermOnce   -> pure True
    PermDeny   -> pure False
    PermNo     -> pure False
    PermNever  -> pure False

-- | Helper for UI event handler to resolve a pending prompt
resolvePrompt :: PermissionPrompt -> PermissionLevel -> IO ()
resolvePrompt PermissionPrompt{..} level =
  atomically $ putTMVar promptReply level
