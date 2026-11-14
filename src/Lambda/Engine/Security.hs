{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Security
  ( SecurityState(..)
  , PolicyRules(..)
  , CallerGrantContext(..)
  , SecuritySubject(..)
  , AuthEvaluation(..)
  , initSecurity
  , initSecurityWithRoot
  , matchGlob
  , splitShellPipeline
  , hasUnsafeShellSyntax
  , stripCommandWrappers
  , deriveSecurityGlob
  , deriveSubjectRules
  , subjectDisplayText
  , applyDecision
  , evaluateSubject
  , evaluatePolicy
  , checkAuthorization
  , checkAuthorizationSubject
  , resolvePrompt
  ) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lambda.Config (defaultAllowGlobs, persistSecurityRule)
import Lambda.Types

-- | Pure snapshot of all active allow/deny policy rules
data PolicyRules = PolicyRules
  { prAlwaysDeny   :: !(Set Text)
  , prSessionDeny  :: !(Set Text)
  , prAlwaysAllow  :: !(Set Text)
  , prSessionAllow :: !(Set Text)
  } deriving stock (Eq, Show, Generic)

-- | Caller capability context for pure policy evaluation
data CallerGrantContext = CallerGrantContext
  { cgcCaller    :: !CallerContext
  , cgcSubPerms  :: !SubAgentPermissions
  , cgcAgentMode :: !AgentMode
  } deriving stock (Eq, Show, Generic)

-- | Structured authorization subject (avoids synthetic string re-parsing)
data SecuritySubject
  = SubjBash !Text
  | SubjTool !Text !ToolCapability !(Maybe FilePath) !Bool
  | SubjWebFetch !Text
  deriving stock (Eq, Show, Generic)

-- | Pure evaluation result before any STM/IO prompting
data AuthEvaluation
  = EvalAllow
  | EvalDeny !AuthOutcome
  | EvalRequirePrompt ![Text] !Text
  deriving stock (Eq, Show, Generic)

data SecurityState = SecurityState
  { securityWorkspaceRoot :: !FilePath
  , policyRules           :: !(TVar PolicyRules)
  , uiPromptQueue         :: !(TBQueue PermissionPrompt)
  , promptCounter         :: !(TVar Int)
  , activePrompts         :: !(TVar (Map Int Text))
  , pendingWaiters        :: !(TVar (Map Int ([Text], TMVar PermissionResponse)))
  , onPromptResolved      :: !(Int -> PermissionResponse -> IO ())
  , onPromptStatus        :: !(CallerContext -> Bool -> IO ())
  }

-- | Slash-agnostic wildcard matcher ('*' matches any 0+ chars including '/', '?' matches 1 char)
matchGlob :: Text -> Text -> Bool
matchGlob pat txt = go (T.unpack pat) (T.unpack txt)
  where
    go [] [] = True
    go ('*':ps) str = go ps str || case str of
      []     -> False
      (_:ss) -> go ('*':ps) ss
    go ('?':ps) (_:ss) = go ps ss
    go (p:ps) (s:ss) | p == s = go ps ss
    go _ _ = False

matchAnyRule :: Set Text -> Text -> Maybe Text
matchAnyRule rules target =
  case filter (`matchGlob` target) (Set.toList rules) of
    (r:_) -> Just r
    []    -> Nothing

-- | Quote-aware shell pipeline splitter on unquoted ;, &&, ||, |, &, and newlines
splitShellPipeline :: Text -> [Text]
splitShellPipeline raw =
  let segs = map T.strip (go False False False (T.unpack raw))
  in filter (not . T.null) segs
  where
    go _ _ _ [] = [""]
    go sq dq esc (c:cs)
      | esc = cons c (go sq dq False cs)
      | not sq && c == '\\' = cons c (go sq dq True cs)
      | not dq && c == '\'' = cons c (go (not sq) dq False cs)
      | not sq && c == '"'  = cons c (go sq (not dq) False cs)
      | not sq && not dq = case c of
          ';'  -> "" : go False False False cs
          '\n' -> "" : go False False False cs
          '|'  -> case cs of
                    ('|':rest) -> "" : go False False False rest
                    _          -> "" : go False False False cs
          '&'  -> case cs of
                    ('&':rest) -> "" : go False False False rest
                    _          -> "" : go False False False cs
          _    -> cons c (go False False False cs)
      | otherwise = cons c (go sq dq False cs)

    cons c (x:xs) = T.cons c x : xs
    cons c []     = [T.singleton c]

-- | Detects unquoted/double-quoted command substitution, process substitution, or output redirects
hasUnsafeShellSyntax :: Text -> Bool
hasUnsafeShellSyntax raw = go False False False (T.unpack raw)
  where
    go _ _ _ [] = False
    go sq dq esc (c:cs)
      | esc = go sq dq False cs
      | not sq && c == '\\' = go sq dq True cs
      | not dq && c == '\'' = go (not sq) dq False cs
      | not sq && c == '"'  = go sq (not dq) False cs
      | not sq && c == '$' = case cs of
          ('(':_) -> True
          _       -> go sq dq False cs
      | not sq && c == '`' = True
      | not sq && not dq && (c == '<' || c == '>') = case (c, cs) of
          (_, ('(':_)) -> True
          ('>', _)     -> not (isSafeFdRedirect cs)
          _            -> go False False False cs
      | otherwise = go sq dq False cs

    isSafeFdRedirect ('&':'1':_) = True
    isSafeFdRedirect ('&':'2':_) = True
    isSafeFdRedirect _           = False

-- | Strips leading KEY=VAL env assignments and benign wrapper commands
stripCommandWrappers :: Text -> Text
stripCommandWrappers cmd =
  let ws = T.words (T.strip cmd)
  in T.unwords (dropWrappers ws)
  where
    dropWrappers [] = []
    dropWrappers (w:ws)
      | isEnvAssign w = dropWrappers ws
      | w `elem` ["env", "time", "nice", "nohup"] = dropWrappers ws
      | otherwise = w:ws

    isEnvAssign w = case T.breakOn "=" w of
      (k, v) -> not (T.null k) && not (T.null v) && T.all (\ch -> isAlphaNum ch || ch == '_') k

-- | Derive a clean, predictable glob pattern from a command or tool call
deriveSecurityGlob :: Text -> String
deriveSecurityGlob raw =
  let t = T.strip raw
      stripped = stripCommandWrappers t
  in case T.words stripped of
    [] -> "*"
    [w] ->
      if "*" `T.isSuffixOf` w
        then T.unpack w
        else T.unpack w ++ "*"
    (w1:w2:_)
      | w1 == "outside_workspace:" ->
          "outside_workspace: " ++ T.unpack w2 ++ "*"
      | w2 `elem` ["-c", "-e", "--eval", "--run"] ->
          T.unpack stripped
      | w1 `elem` ["git", "cabal", "cargo", "npm", "yarn", "pnpm", "docker", "kubectl", "gh", "nix", "systemctl"] ->
          T.unpack w1 ++ " " ++ T.unpack w2 ++ "*"
      | otherwise ->
          T.unpack w1 ++ "*"

extractUrlDomainPrefix :: Text -> Text
extractUrlDomainPrefix url =
  case T.breakOn "://" url of
    (scheme, rest) | not (T.null rest) ->
      let afterScheme = T.drop 3 rest
          host = T.takeWhile (/= '/') afterScheme
      in if T.null host then url else scheme <> "://" <> host <> "/*"
    _ -> url

deriveSubjectRules :: SecuritySubject -> [Text]
deriveSubjectRules (SubjBash cmd) =
  let segs = case splitShellPipeline cmd of
        [] -> [cmd]
        xs -> xs
  in nubOrd (map (T.pack . deriveSecurityGlob) segs)
deriveSubjectRules (SubjWebFetch url) =
  ["fetch_url " <> extractUrlDomainPrefix url]
deriveSubjectRules (SubjTool name _ mPath isOutside)
  | isOutside = ["outside_workspace: " <> name <> "*"]
  | otherwise = case mPath of
      Just _  -> [name <> "*"]
      Nothing -> [name <> "*"]

subjectDisplayText :: SecuritySubject -> Text
subjectDisplayText (SubjBash cmd) = cmd
subjectDisplayText (SubjWebFetch url) = "fetch_url " <> url
subjectDisplayText (SubjTool name _ mPath isOutside) =
  let prefix = if isOutside then "outside_workspace: " else ""
  in case mPath of
       Just p  -> prefix <> name <> " " <> T.pack p
       Nothing -> prefix <> name

-- | Pure policy state transition
applyDecision :: PermissionResponse -> [Text] -> PolicyRules -> PolicyRules
applyDecision PermissionResponse{..} rules pr@PolicyRules{..} =
  let ruleSet = Set.fromList rules
  in case (permDecision, permScope) of
       (_, Once)        -> pr
       (Allow, Session) -> pr { prSessionAllow = Set.union ruleSet prSessionAllow }
       (Deny, Session)  -> pr { prSessionDeny  = Set.union ruleSet prSessionDeny }
       (Allow, Always)  -> pr { prAlwaysAllow  = Set.union ruleSet prAlwaysAllow }
       (Deny, Always)   -> pr { prAlwaysDeny   = Set.union ruleSet prAlwaysDeny }

defaultSafeRules :: [Text]
defaultSafeRules = map T.pack defaultAllowGlobs

-- | Evaluate a single atomic subject against PolicyRules and CallerGrantContext
evaluateSubject :: PolicyRules -> CallerGrantContext -> SecuritySubject -> AuthEvaluation
evaluateSubject PolicyRules{..} CallerGrantContext{..} subj =
  case subj of
    SubjBash cmd ->
      let segs = case splitShellPipeline cmd of
            [] -> [cmd]
            xs -> xs
      in evaluateBashSegments cmd segs
    SubjWebFetch url ->
      let target = "fetch_url " <> url
          rule = "fetch_url " <> extractUrlDomainPrefix url
      in checkTarget target rule False ReadOnly True
    SubjTool name cap _ isOut ->
      let target = subjectDisplayText subj
          rule = T.pack (deriveSecurityGlob target)
      in checkTarget target rule isOut cap False
  where
    checkTarget target rule isOut cap isFetch =
      case checkDeny target of
        Just out -> EvalDeny out
        Nothing
          | cgcAgentMode == PlanMode && cap == Destructive ->
              EvalDeny (DeniedByPlanMode target)
          | isOut ->
              checkExplicitAllow target rule
          | isFetch ->
              checkExplicitAllow target rule
          | otherwise ->
              case checkExplicitAllow target rule of
                EvalAllow -> EvalAllow
                _ -> case cgcCaller of
                  MainAgent
                    | cap == ReadOnly -> EvalAllow
                    | otherwise -> EvalRequirePrompt [rule] target
                  SubAgentId _ _
                    | cap == ReadOnly && matchesSubAllow target -> EvalAllow
                    | otherwise -> EvalRequirePrompt [rule] target

    checkDeny target
      | Just r <- matchAnyRule prAlwaysDeny target  = Just (DeniedByPolicy Always r)
      | Just r <- matchAnyRule prSessionDeny target = Just (DeniedByPolicy Session r)
      | SubAgentId _ _ <- cgcCaller
      , Just r <- matchAnyRule (sapDeny cgcSubPerms) target = Just (DeniedByPolicy Session r)
      | otherwise = Nothing

    checkExplicitAllow target rule
      | Just _ <- matchAnyRule prAlwaysAllow target  = EvalAllow
      | Just _ <- matchAnyRule prSessionAllow target = EvalAllow
      | otherwise = EvalRequirePrompt [rule] target

    matchesSubAllow target = isJust (matchAnyRule (sapAllow cgcSubPerms) target)

    isSafeDefault seg =
      not (hasUnsafeShellSyntax seg)
      && any (`matchGlob` stripCommandWrappers seg) defaultSafeRules

    evaluateBashSegments fullCmd segs =
      let segEvals = map evalSeg segs
      in case [ out | EvalDeny out <- segEvals ] of
           (out:_) -> EvalDeny out
           [] ->
             let neededRules = nubOrd [ r | EvalRequirePrompt rs _ <- segEvals, r <- rs ]
             in if null neededRules
                  then EvalAllow
                  else EvalRequirePrompt neededRules fullCmd

    evalSeg seg =
      let cleanSeg = stripCommandWrappers seg
          rule = T.pack (deriveSecurityGlob cleanSeg)
          unsafe = hasUnsafeShellSyntax seg
      in case checkDeny cleanSeg of
           Just out -> EvalDeny out
           Nothing
             | cgcAgentMode == PlanMode && (unsafe || not (isSafeDefault seg)) ->
                 EvalDeny (DeniedByPlanMode seg)
             | unsafe ->
                 case checkExplicitAllow cleanSeg rule of
                   EvalAllow -> EvalAllow
                   _         -> EvalRequirePrompt [rule] seg
             | Just _ <- matchAnyRule prAlwaysAllow cleanSeg  -> EvalAllow
             | Just _ <- matchAnyRule prSessionAllow cleanSeg -> EvalAllow
             | SubAgentId _ _ <- cgcCaller ->
                 if matchesSubAllow cleanSeg && (isSafeDefault seg || not (isGenericBashGrant))
                   then EvalAllow
                   else EvalRequirePrompt [rule] seg
             | isSafeDefault seg -> EvalAllow
             | otherwise -> EvalRequirePrompt [rule] seg

    isGenericBashGrant = any (\g -> g == "bash" || g == "bash*") (Set.toList (sapAllow cgcSubPerms))

    isJust (Just _) = True
    isJust Nothing  = False

-- | Convenience pure evaluator using default ExecMode and empty subagent grants
evaluatePolicy :: PolicyRules -> CallerGrantContext -> SecuritySubject -> AuthEvaluation
evaluatePolicy = evaluateSubject

initSecurityWithRoot :: FilePath -> [String] -> [String] -> IO SecurityState
initSecurityWithRoot wsRoot allowList denyList = do
  let initRules = PolicyRules
        { prAlwaysDeny   = Set.fromList (map T.pack denyList)
        , prSessionDeny  = Set.empty
        , prAlwaysAllow  = Set.fromList (map T.pack allowList)
        , prSessionAllow = Set.empty
        }
  tRules   <- newTVarIO initRules
  queue    <- newTBQueueIO 64
  pCount   <- newTVarIO 1
  aPrompts <- newTVarIO Map.empty
  waiters  <- newTVarIO Map.empty
  pure SecurityState
    { securityWorkspaceRoot = wsRoot
    , policyRules           = tRules
    , uiPromptQueue         = queue
    , promptCounter         = pCount
    , activePrompts         = aPrompts
    , pendingWaiters        = waiters
    , onPromptResolved      = \_ _ -> pure ()
    , onPromptStatus        = \_ _ -> pure ()
    }

initSecurity :: [String] -> [String] -> IO SecurityState
initSecurity = initSecurityWithRoot "."

-- | String-based authorization entry point (evaluates as bash/command subject)
checkAuthorization :: SecurityState -> CallerContext -> Text -> Aeson.Value -> Maybe Text -> IO AuthOutcome
checkAuthorization sec caller cmd payload mToolCallId =
  let ctx = CallerGrantContext caller emptySubAgentPermissions ExecMode
  in checkAuthorizationSubject sec ctx (SubjBash cmd) payload mToolCallId

-- | Structured authorization entry point with STM snapshot consistency and bracketed prompt tracking
checkAuthorizationSubject
  :: SecurityState
  -> CallerGrantContext
  -> SecuritySubject
  -> Aeson.Value
  -> Maybe Text
  -> IO AuthOutcome
checkAuthorizationSubject sec@SecurityState{..} ctx@CallerGrantContext{..} subj payload mToolCallId = do
  rules <- readTVarIO policyRules
  case evaluateSubject rules ctx subj of
    EvalAllow    -> pure Authorized
    EvalDeny out -> pure out
    EvalRequirePrompt _ _ -> promptUserForSubject
  where
    toolLabel = case subj of
      SubjBash _          -> "bash"
      SubjWebFetch _      -> "fetch_url"
      SubjTool nm _ _ _   -> nm

    promptUserForSubject =
      bracket acquirePrompt releasePrompt usePrompt
      where
        acquirePrompt = atomically $ do
          curRules <- readTVar policyRules
          case evaluateSubject curRules ctx subj of
            EvalAllow    -> pure (Left Authorized)
            EvalDeny out -> pure (Left out)
            EvalRequirePrompt reqRules disp -> do
              allWaiters <- readTVar pendingWaiters
              let findMatching [] = Nothing
                  findMatching ((wId, (wRules, wVar)):rest)
                    | not (null reqRules) && all (`elem` wRules) reqRules = Just (wId, wVar)
                    | otherwise = findMatching rest
              case findMatching (Map.toList allWaiters) of
                Just (wId, wVar) ->
                  pure (Right (wId, reqRules, disp, wVar, False))
                Nothing -> do
                  rId <- readTVar promptCounter
                  writeTVar promptCounter (rId + 1)
                  reply <- newEmptyTMVar
                  let req = PermissionPrompt rId cgcCaller toolLabel payload reqRules reply mToolCallId
                  writeTBQueue uiPromptQueue req
                  modifyTVar' activePrompts (Map.insert rId disp)
                  modifyTVar' pendingWaiters (Map.insert rId (reqRules, reply))
                  pure (Right (rId, reqRules, disp, reply, True))

        releasePrompt (Left _) = pure ()
        releasePrompt (Right (rId, _, _, replyVar, isOwner))
          | not isOwner = onPromptStatus cgcCaller False
          | otherwise = do
              wasResolved <- atomically $ do
                modifyTVar' activePrompts (Map.delete rId)
                modifyTVar' pendingWaiters (Map.delete rId)
                empty <- isEmptyTMVar replyVar
                when empty $ void $ tryPutTMVar replyVar (PermissionResponse Deny Once)
                pure (not empty)
              onPromptStatus cgcCaller False
              unless wasResolved $
                onPromptResolved rId (PermissionResponse Deny Once)

        usePrompt (Left out) = pure out
        usePrompt (Right (rId, reqRules, disp, replyVar, isOwner)) = do
          onPromptStatus cgcCaller True
          choice <- atomically $ readTMVar replyVar

          when isOwner $ do
            case permScope choice of
              Always -> do
                _ <- persistSecurityRule securityWorkspaceRoot (permDecision choice) (map T.unpack reqRules)
                pure ()
              _ -> pure ()

            autoResolved <- atomically $ do
              modifyTVar' policyRules (applyDecision choice reqRules)
              if permScope choice == Once
                then pure []
                else do
                  updatedRules <- readTVar policyRules
                  allWaiters <- readTVar pendingWaiters
                  let checkWaiter (wId, (wRules, wVar)) =
                        if wId /= rId && all (`elem` reqRules) wRules
                          then Just (wId, wVar)
                          else case evaluateSubject updatedRules ctx (SubjBash (T.intercalate " && " wRules)) of
                            EvalAllow -> if permDecision choice == Allow then Just (wId, wVar) else Nothing
                            EvalDeny _ -> if permDecision choice == Deny then Just (wId, wVar) else Nothing
                            _ -> Nothing
                  let matches = [ m | Just m <- map checkWaiter (Map.toList allWaiters) ]
                  forM_ matches $ \(_, wVar) -> void $ tryPutTMVar wVar choice
                  pure (map fst matches)

            forM_ autoResolved $ \wId ->
              onPromptResolved wId choice

          case permDecision choice of
            Allow -> pure Authorized
            Deny  -> pure (DeniedByUser (permScope choice) disp)

-- | Helper for UI/ACP event handler to resolve a pending prompt
resolvePrompt :: PermissionPrompt -> PermissionResponse -> IO ()
resolvePrompt PermissionPrompt{..} resp =
  atomically $ void $ tryPutTMVar promptReply resp
