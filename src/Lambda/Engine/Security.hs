{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Security
  ( SecurityState(..)
  , initSecurity
  , checkAuthorization
  , requestGrantApproval
  , resolvePrompt
  ) where

import Control.Concurrent.STM
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath.Glob (Pattern, compile, match)
import Lambda.Types

data SecurityState = SecurityState
  { alwaysAllowedPatterns :: ![String]
  , alwaysDeniedPatterns  :: ![String]
  , alwaysAllowedCompiled :: ![Pattern]
  , alwaysDeniedCompiled  :: ![Pattern]
  , sessionAllowList      :: !(TVar (Set Text))
  , sessionDenyList       :: !(TVar (Set Text))
  , uiPromptQueue         :: !(TBQueue PermissionPrompt)
  , promptCounter         :: !(TVar Int)
  }

initSecurity :: [String] -> [String] -> IO SecurityState
initSecurity allowList denyList = do
  sAllow <- newTVarIO Set.empty
  sDeny  <- newTVarIO Set.empty
  queue  <- newTBQueueIO 64
  pCount <- newTVarIO 1
  let cAllow = map compile allowList
      cDeny  = map compile denyList
  pure SecurityState
    { alwaysAllowedPatterns = allowList
    , alwaysDeniedPatterns  = denyList
    , alwaysAllowedCompiled = cAllow
    , alwaysDeniedCompiled  = cDeny
    , sessionAllowList      = sAllow
    , sessionDenyList       = sDeny
    , uiPromptQueue         = queue
    , promptCounter         = pCount
    }

-- | Pragmatic UX safeguard check with non-blocking STM queueing
checkAuthorization :: SecurityState -> CallerContext -> Text -> Aeson.Value -> IO Bool
checkAuthorization SecurityState{..} caller cmd payload = do
  let cmdStr = T.unpack cmd

  -- Step 1: Static Deny List (Deny-first rule)
  if any (`match` cmdStr) alwaysDeniedCompiled
    then pure False
    else do
      -- Step 2: Static Allow List (repetitive safe operations)
      if any (`match` cmdStr) alwaysAllowedCompiled
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
                (_reqId, replyVar) <- atomically $ do
                  rId <- readTVar promptCounter
                  writeTVar promptCounter (rId + 1)
                  reply <- newEmptyTMVar
                  let req = PermissionPrompt rId caller cmd payload reply
                  writeTBQueue uiPromptQueue req
                  pure (rId, reply)

                choice <- atomically $ takeTMVar replyVar
                case choice of
                  PermAlways -> do
                    atomically $ modifyTVar' sessionAllowList (Set.insert cmd)
                    pure True
                  PermOnce -> pure True
                  PermNo   -> pure False
                  PermNever -> do
                    atomically $ modifyTVar' sessionDenyList (Set.insert cmd)
                    pure False

-- | Upfront authorization prompt when spawning a subagent with requested capabilities
requestGrantApproval :: SecurityState -> Int -> Text -> [Text] -> IO Bool
requestGrantApproval SecurityState{..} sId hypothesis requestedGlobs = do
  let grantDesc = "SubAgent #" <> T.pack (show sId) <> " capability grant"
      payload   = Aeson.object
        [ "subagent_id"  Aeson..= sId
        , "hypothesis"   Aeson..= hypothesis
        , "capabilities" Aeson..= requestedGlobs
        ]

  (_reqId, replyVar) <- atomically $ do
    rId <- readTVar promptCounter
    writeTVar promptCounter (rId + 1)
    reply <- newEmptyTMVar
    let req = PermissionPrompt rId (SubAgentId sId hypothesis) grantDesc payload reply
    writeTBQueue uiPromptQueue req
    pure (rId, reply)

  choice <- atomically $ takeTMVar replyVar
  case choice of
    PermAlways -> do
      atomically $ modifyTVar' sessionAllowList (\s -> foldr Set.insert s requestedGlobs)
      pure True
    PermOnce   -> pure True
    PermNo     -> pure False
    PermNever  -> pure False

-- | Helper for UI event handler to resolve a pending prompt
resolvePrompt :: PermissionPrompt -> PermissionLevel -> IO ()
resolvePrompt PermissionPrompt{..} level =
  atomically $ putTMVar promptReply level
