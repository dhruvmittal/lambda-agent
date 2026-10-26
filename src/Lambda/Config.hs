{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Config
  ( Config(..)
  , McpServerConfig(..)
  , SpecialistConfig(..)
  , defaultConfig
  , defaultSpecialists
  , defaultModelAliases
  , resolveModelAlias
  , resolveModelWithConfig
  , isLocalEndpoint
  , isOpenAiEndpoint
  , isOpenRouterEndpoint
  , formatEndpointBadge
  , deriveProviderFromUrl
  , extractProviderAndModel
  , lookupModelContextLimit
  , curatedModels
  , resolveEnvTemplates
  , loadConfig
  , parseMarkdownAgent
  , loadMarkdownAgentsFromDir
  , loadAllMarkdownAgents
  , persistAllowGlob
  , persistTheme
  ) where

import Control.Applicative ((<|>))
import Control.Exception (catch, IOException)
import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (nub, isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:?), (.!=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, takeExtension, (</>))
import Text.Read (readMaybe)

data McpServerConfig = McpServerConfig
  { mcpCommand :: !FilePath
  , mcpArgs    :: ![Text]
  , mcpEnv     :: !(Map Text Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON McpServerConfig where
  toJSON McpServerConfig{..} = Aeson.object
    [ "command" .= mcpCommand
    , "args"    .= mcpArgs
    , "env"     .= mcpEnv
    ]

instance Aeson.FromJSON McpServerConfig where
  parseJSON = Aeson.withObject "McpServerConfig" $ \obj -> do
    mcpCommand <- obj .:? "command" .!= ""
    mcpArgs    <- obj .:? "args" .!= []
    mcpEnv     <- obj .:? "env" .!= Map.empty
    pure McpServerConfig{..}

data SpecialistConfig = SpecialistConfig
  { specialistDescription  :: !Text
  , specialistPrompt       :: !Text
  , specialistBudget       :: !Int
  , specialistCapabilities :: ![Text]
  , specialistDenyGlobs    :: ![Text]
  , specialistModel        :: !(Maybe Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON SpecialistConfig where
  toJSON SpecialistConfig{..} = Aeson.object $
    [ "description"  .= specialistDescription
    , "prompt"       .= specialistPrompt
    , "budget"       .= specialistBudget
    , "capabilities" .= specialistCapabilities
    , "deny_globs"   .= specialistDenyGlobs
    ] ++ maybe [] (\m -> ["model" .= m]) specialistModel

instance Aeson.FromJSON SpecialistConfig where
  parseJSON = Aeson.withObject "SpecialistConfig" $ \obj -> do
    specialistDescription  <- obj .:? "description" .!= ""
    specialistPrompt       <- obj .:? "prompt" .!= ""
    mBud                   <- obj .:? "budget"
    mSteps                 <- obj .:? "steps"
    let specialistBudget = fromMaybe 6 (mBud <|> mSteps)
    specialistCapabilities <- obj .:? "capabilities" .!= []
    specialistDenyGlobs    <- obj .:? "deny_globs" .!= []
    specialistModel        <- obj .:? "model"
    pure SpecialistConfig{..}

defaultSpecialists :: Map Text SpecialistConfig
defaultSpecialists = Map.fromList
  [ ( "surveyor"
    , SpecialistConfig
        { specialistDescription  = "Read-only codebase explorer & caller graph mapper"
        , specialistPrompt       = T.unlines
            [ "# Role: Codebase Surveyor & Architecture Mapper"
            , "You are a fastidious, read-only codebase explorer and topological mapping specialist."
            , "Your mission is to map symbols, locate definitions, trace imports, and verify architectural invariants."
            , "Operational Invariants:"
            , "1. STRICT READ-ONLY: Never modify workspace files. Only read-only tools are permitted."
            , "2. NO PATH HALLUCINATION: Never guess paths or symbols. Always verify via find_by_name, grep_search, or sd_read/sd_recall."
            , "3. PRECISE REFERENCES: Return exact file paths and line ranges (e.g. src/Foo.hs:L40-L65)."
            , "When your survey is complete, call `submit_report` with status, summary, details, and any artifact path."
            ]
        , specialistBudget       = 16
        , specialistCapabilities = ["read_file*", "list_directory*", "grep_search*", "find_by_name*", "fetch_url*", "sd_*", "git status*", "git diff*", "git log*"]
        , specialistDenyGlobs    = []
        , specialistModel        = Nothing
        }
    )
  , ( "debugger"
    , SpecialistConfig
        { specialistDescription  = "Forensic bug investigator & minimal test reproducer"
        , specialistPrompt       = T.unlines
            [ "# Role: Forensic Systems Debugger"
            , "You are a forensic debugging specialist. Your mission is to isolate the root-cause of crashes, test failures, and regressions."
            , "Operational Invariants:"
            , "1. HYPOTHESIS TESTING: Formulate distinct, testable hypotheses for the failure mechanism."
            , "2. STACK ISOLATION: Locate the exact offending line, frame, or memory violation."
            , "3. MINIMAL SURGERY: Diagnose the root cause and propose the minimal surgical fix required."
            , "When your diagnosis is complete, call `submit_report` with status, summary, details, and any artifact path."
            ]
        , specialistBudget       = 14
        , specialistCapabilities = ["read_file*", "grep_search*", "find_by_name*", "cabal test*", "ctest*", "pytest*", "bash*", "sd_*"]
        , specialistDenyGlobs    = []
        , specialistModel        = Nothing
        }
    )
  , ( "profiler"
    , SpecialistConfig
        { specialistDescription  = "Systems performance, Valgrind, and hotspot analyzer"
        , specialistPrompt       = T.unlines
            [ "# Role: Systems Performance Profiler"
            , "You are a systems performance profiling specialist. Your mission is to benchmark and identify CPU, memory, and cache bottlenecks."
            , "Operational Invariants:"
            , "1. METRIC PRECISION: Run profilers (valgrind callgrind/massif, perf, RTS profiling) and extract instruction/cycle metrics."
            , "2. HOTSPOT ISOLATION: Identify the top 3 functions consuming the majority of time/allocations."
            , "3. CONCRETE REMEDY: Recommend specific optimizations (e.g. allocation pre-sizing, unboxed structures, memory reuse)."
            , "When profiling is complete, call `submit_report` with status, summary, details, and the profile artifact path."
            ]
        , specialistBudget       = 10
        , specialistCapabilities = ["valgrind*", "callgrind_annotate*", "perf*", "read_file*", "bash*", "cabal bench*"]
        , specialistDenyGlobs    = []
        , specialistModel        = Nothing
        }
    )
  , ( "implementer"
    , SpecialistConfig
        { specialistDescription  = "Surgical file modifier & refactoring implementer"
        , specialistPrompt       = T.unlines
            [ "# Role: Surgical Implementation Specialist"
            , "You are an implementation specialist. Your mission is to execute clean, minimal file modifications based on approved plans."
            , "Operational Invariants:"
            , "1. SURGICAL PRECISION: Make only the targeted changes required. Do not refactor unrelated code."
            , "2. PRESERVE DOCUMENTATION: Never delete comments, docstrings, or existing architectural conventions."
            , "3. VERIFY DIFFS: Check your changes for syntax correctness and clean formatting."
            , "When changes are complete, call `submit_report` with status, summary, details, and the modified file paths."
            ]
        , specialistBudget       = 12
        , specialistCapabilities = ["write_file*", "replace_lines*", "read_file*", "sd_read*"]
        , specialistDenyGlobs    = []
        , specialistModel        = Nothing
        }
    )
  , ( "reviewer"
    , SpecialistConfig
        { specialistDescription  = "Adversarial pre-commit invariant and simplicity auditor"
        , specialistPrompt       = T.unlines
            [ "# Role: Adversarial Code Reviewer"
            , "You are an adversarial systems code reviewer. Your mission is to verify safety, correctness, and simplicity before code lands."
            , "Operational Invariants:"
            , "1. INVARIANT CHECKING: Inspect git diffs against architectural invariants, concurrency safety, and memory management."
            , "2. YAGNI & SIMPLICITY: Hunt for over-engineering, unneeded dependencies, speculative abstractions, and dead flexibility."
            , "3. ACTIONABLE VERDICT: Issue APPROVED or CHANGES_REQUESTED with precise line-by-line feedback."
            , "When review is complete, call `submit_report` with status, summary, details, and any review artifacts."
            ]
        , specialistBudget       = 12
        , specialistCapabilities = ["git diff*", "git log*", "read_file*", "cabal test*", "ctest*", "sd_recall*"]
        , specialistDenyGlobs    = []
        , specialistModel        = Nothing
        }
    )
  ]

data Config = Config
  { apiBaseUrl          :: !Text
  , apiKey              :: !Text
  , modelName           :: !Text
  , customHeaders       :: !(Map Text Text)
  , alwaysAllowGlobs    :: ![String]
  , alwaysDenyGlobs     :: ![String]
  , workspaceRoot       :: !FilePath
  , artifactDir         :: !FilePath
  , maxTurnBudget       :: !Int
  , contextWindowLimit  :: !Int
  , mcpServers          :: !(Map Text McpServerConfig)
  , specialists         :: !(Map Text SpecialistConfig)
  , maxSavedSessions    :: !Int
  , modelAliases        :: !(Map Text Text)
  , configuredModels    :: ![Text]
  , themeName           :: !Text
  , customThemes        :: !(Map Text (Map Text Text))
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Config where
  toJSON Config{..} = Aeson.object
    [ "api_base_url"        .= apiBaseUrl
    , "api_key"             .= apiKey
    , "model_name"          .= modelName
    , "custom_headers"      .= customHeaders
    , "always_allow_globs"  .= alwaysAllowGlobs
    , "always_deny_globs"   .= alwaysDenyGlobs
    , "max_turn_budget"     .= maxTurnBudget
    , "context_limit"       .= contextWindowLimit
    , "mcp_servers"         .= mcpServers
    , "specialists"         .= specialists
    , "max_saved_sessions"  .= maxSavedSessions
    , "model_aliases"       .= modelAliases
    , "models"              .= configuredModels
    , "theme"               .= themeName
    , "themes"              .= customThemes
    ]

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \obj -> do
    apiBaseUrl         <- obj .:? "api_base_url" .!= ""
    apiKey             <- obj .:? "api_key" .!= ""
    modelName          <- obj .:? "model_name" .!= ""
    customHeaders      <- obj .:? "custom_headers" .!= Map.empty
    alwaysAllowGlobs   <- obj .:? "always_allow_globs" .!= defaultAllowGlobs
    alwaysDenyGlobs    <- obj .:? "always_deny_globs" .!= defaultDenyGlobs
    maxTurnBudget      <- obj .:? "max_turn_budget" .!= 30
    contextWindowLimit <- obj .:? "context_limit" .!= 128000
    mcpServers         <- obj .:? "mcp_servers" .!= Map.empty
    userSpecialists    <- obj .:? "specialists" .!= Map.empty
    userSubagents      <- obj .:? "subagents" .!= Map.empty
    maxSavedSessions   <- obj .:? "max_saved_sessions" .!= 50
    mUserAliases       <- obj .:? "model_aliases"
    modelAliases       <- case mUserAliases of
                            Just a  -> pure a
                            Nothing -> obj .:? "aliases" .!= Map.empty
    configuredModels   <- obj .:? "models" .!= []
    themeName          <- obj .:? "theme" .!= "default"
    customThemes       <- obj .:? "themes" .!= Map.empty
    let specialists = Map.union userSpecialists (Map.union userSubagents defaultSpecialists)
        workspaceRoot  = "."
        artifactDir    = ".lambda/artifacts"
    pure Config{..}

defaultAllowGlobs :: [String]
defaultAllowGlobs =
  [ "git status*"
  , "git diff*"
  , "git log*"
  , "ls*"
  , "pwd*"
  , "cat *"
  , "read_file*"
  , "list_directory*"
  , "spawn_specialist_subagent*"
  , "resume_subagent*"
  , "sd_*"
  ]

defaultDenyGlobs :: [String]
defaultDenyGlobs =
  [ "rm -rf /*"
  , "mkfs*"
  , ":(){ :|:& };:"
  ]

defaultConfig :: Config
defaultConfig = Config
  { apiBaseUrl          = ""
  , apiKey              = ""
  , modelName           = ""
  , customHeaders       = Map.empty
  , alwaysAllowGlobs    = defaultAllowGlobs
  , alwaysDenyGlobs     = defaultDenyGlobs
  , workspaceRoot       = "."
  , artifactDir         = ".lambda/artifacts"
  , maxTurnBudget       = 30
  , contextWindowLimit  = 128000
  , mcpServers          = Map.empty
  , specialists         = defaultSpecialists
  , maxSavedSessions    = 50
  , modelAliases        = Map.empty
  , configuredModels    = []
  , themeName           = "default"
  , customThemes        = Map.empty
  }

-- | Default capabilities granted to OpenCode markdown agents unless restricted by frontmatter
defaultOpenCodeCapabilities :: [Text]
defaultOpenCodeCapabilities =
  [ "read_file*"
  , "list_directory*"
  , "grep_search*"
  , "find_by_name*"
  , "fetch_url*"
  , "replace_lines*"
  , "edit_file*"
  , "write_file*"
  , "bash*"
  , "sd_*"
  , "git status*"
  , "git diff*"
  , "git log*"
  ]

-- | Map an OpenCode tool class or permission key to lambdA capability globs
mapOpenCodeToolKey :: Text -> [Text]
mapOpenCodeToolKey k =
  case T.toLower (T.strip k) of
    "edit"      -> ["replace_lines*", "edit_file*"]
    "patch"     -> ["replace_lines*", "edit_file*"]
    "multiedit" -> ["replace_lines*", "edit_file*"]
    "write"     -> ["write_file*"]
    "bash"      -> ["bash*"]
    "shell"     -> ["bash*"]
    "read"      -> ["read_file*", "sd_read*"]
    "list"      -> ["list_directory*"]
    "grep"      -> ["grep_search*"]
    "glob"      -> ["find_by_name*"]
    "webfetch"  -> ["fetch_url*"]
    "websearch" -> ["fetch_url*"]
    other
      | T.null other -> []
      | "*" `T.isInfixOf` other || " " `T.isInfixOf` other -> [T.strip k]
      | otherwise -> [T.strip k <> "*"]

-- | Strip surrounding quotes and trailing inline YAML comments
cleanYamlScalar :: Text -> Text
cleanYamlScalar raw =
  let s = T.strip raw
  in case T.uncons s of
       Just ('"', rest) ->
         T.takeWhile (/= '"') rest
       Just ('\'', rest) ->
         T.takeWhile (/= '\'') rest
       _ ->
         T.strip (fst (T.breakOn " #" s))

-- | Scanner state for parsing frontmatter without sprawling tuples
data FmScannerState = FmScannerState
  { fmSection      :: !Text
  , fmSubSection   :: !Text
  , fmDesc         :: !Text
  , fmBudget       :: !Int
  , fmModel        :: !(Maybe Text)
  , fmHidden       :: !Bool
  , fmCaps         :: ![Text]
  , fmAllowGlobs   :: ![Text]
  , fmDenyGlobs    :: ![Text]
  , fmCmdDenyGlobs :: ![Text]
  , fmBashSpecific :: !Bool
  , fmWildDeny     :: !Bool
  }

defaultScannerState :: FmScannerState
defaultScannerState = FmScannerState
  { fmSection      = ""
  , fmSubSection   = ""
  , fmDesc         = ""
  , fmBudget       = 12
  , fmModel        = Nothing
  , fmHidden       = False
  , fmCaps         = []
  , fmAllowGlobs   = []
  , fmDenyGlobs    = []
  , fmCmdDenyGlobs = []
  , fmBashSpecific = False
  , fmWildDeny     = False
  }

-- | Parse an inline YAML array like ["read_file*", "bash*"]
parseInlineYamlList :: Text -> [Text]
parseInlineYamlList raw =
  let s = T.strip raw
  in case Aeson.decodeStrict (TE.encodeUtf8 s) of
       Just (xs :: [Text]) -> filter (not . T.null) (map cleanYamlScalar xs)
       Nothing ->
         if "[" `T.isPrefixOf` s && "]" `T.isSuffixOf` s
           then
             let inner = T.drop 1 (T.dropEnd 1 s)
                 parts = map cleanYamlScalar (T.splitOn "," inner)
             in filter (not . T.null) parts
           else []

-- | Parse an OpenCode or native lambdA Markdown agent file with YAML frontmatter
parseMarkdownAgent :: Text -> Maybe SpecialistConfig
parseMarkdownAgent rawContent =
  let contentNoBom = fromMaybe rawContent (T.stripPrefix "\xFEFF" rawContent)
      allLines = map (T.dropWhileEnd (== '\r')) (T.lines contentNoBom)
      nonBlankStart = dropWhile (T.null . T.strip) allLines
      (fmLines, bodyLines) = case nonBlankStart of
        (firstLine : rest) | T.strip firstLine == "---" ->
          let (fm, afterFm) = break (\l -> let st = T.strip l in st == "---" || st == "...") rest
          in case afterFm of
               (_closing : body) -> (fm, body)
               []                -> ([], allLines)
        _ -> ([], allLines)
      rawBody = T.strip (T.unlines bodyLines)
  in if T.null rawBody && null fmLines
       then Nothing
       else
         let step st line =
               let trimmed = T.strip line
                   indent = T.length (T.takeWhile isSpace line)
               in if T.null trimmed || "#" `T.isPrefixOf` trimmed
                    then st
                    else if indent == 0
                      then
                        let (kRaw, vWithColon) = T.breakOn ":" trimmed
                            k = T.toLower (T.strip kRaw)
                            v = cleanYamlScalar (T.drop 1 vWithColon)
                        in case k of
                             "description" -> st { fmSection = "", fmSubSection = "", fmDesc = v }
                             "steps"       -> st { fmSection = "", fmSubSection = "", fmBudget = fromMaybe (fmBudget st) (readMaybe (T.unpack v)) }
                             "maxsteps"    -> st { fmSection = "", fmSubSection = "", fmBudget = fromMaybe (fmBudget st) (readMaybe (T.unpack v)) }
                             "budget"      -> st { fmSection = "", fmSubSection = "", fmBudget = fromMaybe (fmBudget st) (readMaybe (T.unpack v)) }
                             "model"       -> st { fmSection = "", fmSubSection = "", fmModel = if T.null v then fmModel st else Just v }
                             "hidden"      -> st { fmSection = "", fmSubSection = "", fmHidden = fmHidden st || T.toLower v == "true" }
                             "disable"     -> st { fmSection = "", fmSubSection = "", fmHidden = fmHidden st || T.toLower v == "true" }
                             "capabilities" ->
                               let inline = parseInlineYamlList (T.strip (T.drop 1 vWithColon))
                               in st { fmSection = "capabilities", fmSubSection = "", fmCaps = fmCaps st ++ inline }
                             "tools"       -> st { fmSection = "tools", fmSubSection = "" }
                             "permission"  -> st { fmSection = "permission", fmSubSection = "" }
                             "permissions" -> st { fmSection = "permission", fmSubSection = "" }
                             _             -> st { fmSection = "", fmSubSection = "" }
                      else
                        -- Indented line inside a section
                        case fmSection st of
                          "capabilities"
                            | "- " `T.isPrefixOf` trimmed ->
                                let item = cleanYamlScalar (T.drop 2 trimmed)
                                in st { fmCaps = fmCaps st ++ [item | not (T.null item)] }
                          s | s `elem` ["tools", "permission"] ->
                            let (kRaw, vWithColon) = T.breakOn ":" trimmed
                                kClean = cleanYamlScalar kRaw
                                vClean = T.toLower (cleanYamlScalar (T.drop 1 vWithColon))
                            in if T.null vClean && ":" `T.isSuffixOf` trimmed
                                 then st { fmSubSection = T.toLower kClean }
                                 else
                                   let activeSub = if indent <= 2 then "" else fmSubSection st
                                   in if activeSub `elem` ["bash", "shell"]
                                        then
                                          case vClean of
                                            "allow" ->
                                              let pat = if "*" `T.isInfixOf` kClean then kClean else kClean <> "*"
                                              in st { fmAllowGlobs = fmAllowGlobs st ++ [pat], fmBashSpecific = True }
                                            "true" ->
                                              let pat = if "*" `T.isInfixOf` kClean then kClean else kClean <> "*"
                                              in st { fmAllowGlobs = fmAllowGlobs st ++ [pat], fmBashSpecific = True }
                                            "deny" | kClean == "*" ->
                                              st { fmDenyGlobs = fmDenyGlobs st ++ ["bash*"], fmBashSpecific = True }
                                            "deny" ->
                                              let pat = if "*" `T.isInfixOf` kClean then kClean else kClean <> "*"
                                              in st { fmCmdDenyGlobs = fmCmdDenyGlobs st ++ [pat] }
                                            "ask" | kClean == "*" ->
                                              st { fmDenyGlobs = fmDenyGlobs st ++ ["bash*"], fmBashSpecific = True }
                                            "ask" ->
                                              let pat = if "*" `T.isInfixOf` kClean then kClean else kClean <> "*"
                                              in st { fmCmdDenyGlobs = fmCmdDenyGlobs st ++ [pat] }
                                            _ -> st
                                        else
                                          let mapped = mapOpenCodeToolKey kClean
                                          in case vClean of
                                               v | v `elem` ["true", "allow"] ->
                                                 st { fmAllowGlobs = fmAllowGlobs st ++ mapped }
                                               v | v `elem` ["false", "deny"] ->
                                                 if kClean == "*"
                                                   then st { fmWildDeny = True }
                                                   else st { fmDenyGlobs = fmDenyGlobs st ++ mapped }
                                               _ -> st
                          _ -> st

             finalSt = foldl step defaultScannerState fmLines
         in if fmHidden finalSt
              then Nothing
              else
                let explicitCaps = fmCaps finalSt
                    allowed = fmAllowGlobs finalSt
                    denied = fmDenyGlobs finalSt
                    effectiveCaps
                      | not (null explicitCaps) = nub explicitCaps
                      | otherwise =
                          let base = if fmWildDeny finalSt then [] else defaultOpenCodeCapabilities
                              deniedWithBash = if fmBashSpecific finalSt && "bash*" `notElem` allowed
                                                 then "bash*" : denied
                                                 else denied
                              filteredBase = filter (`notElem` deniedWithBash) base
                          in nub (filteredBase ++ allowed)
                    fallbackDesc = case filter (not . T.null . T.strip) (T.lines rawBody) of
                      (l:_) -> T.take 80 (T.dropWhile (\c -> c == '#' || isSpace c) l)
                      []    -> "Custom specialist subagent"
                    effectiveDesc = if T.null (fmDesc finalSt) then fallbackDesc else fmDesc finalSt
                    effectivePrompt =
                      if "submit_report" `T.isInfixOf` rawBody
                        then rawBody
                        else rawBody <> "\n\nWhen your task is complete, call `submit_report` with status, summary, details, and any artifact path."
                in Just SpecialistConfig
                     { specialistDescription  = effectiveDesc
                     , specialistPrompt       = effectivePrompt
                     , specialistBudget       = fmBudget finalSt
                     , specialistCapabilities = effectiveCaps
                     , specialistDenyGlobs    = nub (fmCmdDenyGlobs finalSt)
                     , specialistModel        = fmModel finalSt
                     }

-- | Recursively load Markdown agent definitions (.md) from a directory
loadMarkdownAgentsFromDir :: FilePath -> IO (Map Text SpecialistConfig)
loadMarkdownAgentsFromDir rootDir = do
  exists <- doesDirectoryExist rootDir
  if not exists
    then pure Map.empty
    else go "" rootDir
  where
    go relPrefix currentDir = do
      entries <- listDirectory currentDir `catch` (\(_ :: IOException) -> pure [])
      maps <- forM entries $ \entry -> do
        let fullPath = currentDir </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
          then
            if "." `isPrefixOf` entry
              then pure Map.empty
              else do
                let nextPrefix = if null relPrefix then entry else relPrefix ++ "/" ++ entry
                go nextPrefix fullPath
          else
            if takeExtension entry == ".md"
              then do
                raw <- TIO.readFile fullPath `catch` (\(_ :: IOException) -> pure "")
                case parseMarkdownAgent raw of
                  Nothing -> pure Map.empty
                  Just spec -> do
                    let baseKey = T.toLower (T.pack (dropExtension entry))
                        relKey  = if null relPrefix
                                    then baseKey
                                    else T.toLower (T.pack relPrefix) <> "/" <> baseKey
                    pure $ if relKey == baseKey
                             then Map.singleton baseKey spec
                             else Map.fromList [(relKey, spec), (baseKey, spec)]
              else pure Map.empty
      pure (Map.unions maps)

-- | Load Markdown agent definitions from multiple directories in priority order (first wins)
loadAllMarkdownAgents :: [FilePath] -> IO (Map Text SpecialistConfig)
loadAllMarkdownAgents dirs = do
  maps <- mapM loadMarkdownAgentsFromDir dirs
  pure (Map.unions maps)

loadConfig :: FilePath -> IO Config
loadConfig wsRoot = do
  home <- getHomeDirectory
  let globalPath = home </> ".config" </> "lambdA" </> "config.json"
      localPath  = wsRoot </> ".lambda" </> "config.json"

  -- 1. Load global Markdown agents (OpenCode + lambdA)
  globalMdSpecs <- loadAllMarkdownAgents
    [ home </> ".config" </> "lambdA" </> "agents"
    , home </> ".config" </> "opencode" </> "agents"
    , home </> ".config" </> "opencode" </> "agent"
    ]
  let allGlobalSpecs = Map.union globalMdSpecs defaultSpecialists

  -- 2. Load global config.json
  baseCfg <- do
    gExists <- doesFileExist globalPath
    if gExists
      then do
        content <- BS.readFile globalPath
        pure $ case Aeson.decodeStrict content of
          Just c  -> c { specialists = Map.union (specialists c) allGlobalSpecs }
          Nothing -> defaultConfig { specialists = allGlobalSpecs }
      else pure (defaultConfig { specialists = allGlobalSpecs })

  -- 3. Load workspace Markdown agents (.opencode/agent[s] and .lambda/agents)
  localMdSpecs <- loadAllMarkdownAgents
    [ wsRoot </> ".lambda" </> "agents"
    , wsRoot </> ".opencode" </> "agents"
    , wsRoot </> ".opencode" </> "agent"
    ]
  let baseCfgWithLocalMd = baseCfg
        { specialists = Map.union localMdSpecs (specialists baseCfg)
        }

  -- 4. Load workspace .lambda/config.json
  mergedCfg <- do
    lExists <- doesFileExist localPath
    if lExists
      then do
        content <- BS.readFile localPath
        pure $ case Aeson.decodeStrict content of
          Just (Aeson.Object o) ->
            let pick field fallback = case parseMaybe (.:? field) o of
                  Just (Just v) -> v
                  _             -> fallback
                pickMap field fallback = case parseMaybe (.:? field) o of
                  Just (Just m) -> Map.union m fallback
                  _             -> fallback
                mAliases = case parseMaybe (.:? "model_aliases") o of
                  Just (Just a) -> Map.union a (modelAliases baseCfgWithLocalMd)
                  _             -> case parseMaybe (.:? "aliases") o of
                    Just (Just a) -> Map.union a (modelAliases baseCfgWithLocalMd)
                    _             -> modelAliases baseCfgWithLocalMd
                specsWithSubs = case parseMaybe (.:? "subagents") o of
                  Just (Just sub) -> Map.union sub (specialists baseCfgWithLocalMd)
                  _               -> specialists baseCfgWithLocalMd
            in baseCfgWithLocalMd
              { apiBaseUrl         = pick "api_base_url" (apiBaseUrl baseCfgWithLocalMd)
              , apiKey             = pick "api_key" (apiKey baseCfgWithLocalMd)
              , modelName          = pick "model_name" (modelName baseCfgWithLocalMd)
              , customHeaders      = pickMap "custom_headers" (customHeaders baseCfgWithLocalMd)
              , alwaysAllowGlobs   = pick "always_allow_globs" (alwaysAllowGlobs baseCfgWithLocalMd)
              , alwaysDenyGlobs    = pick "always_deny_globs" (alwaysDenyGlobs baseCfgWithLocalMd)
              , maxTurnBudget      = pick "max_turn_budget" (maxTurnBudget baseCfgWithLocalMd)
              , contextWindowLimit = pick "context_limit" (contextWindowLimit baseCfgWithLocalMd)
              , mcpServers         = pickMap "mcp_servers" (mcpServers baseCfgWithLocalMd)
              , specialists        = pickMap "specialists" specsWithSubs
              , maxSavedSessions   = pick "max_saved_sessions" (maxSavedSessions baseCfgWithLocalMd)
              , modelAliases       = mAliases
              , configuredModels   = pick "models" (configuredModels baseCfgWithLocalMd)
              , themeName          = pick "theme" (themeName baseCfgWithLocalMd)
              , customThemes       = pickMap "themes" (customThemes baseCfgWithLocalMd)
              }
          _ -> baseCfgWithLocalMd
      else pure baseCfgWithLocalMd

  -- Load local .env / .env.local variables if present
  dotEnvMap <- do
    let localEnvPath = wsRoot </> ".env.local"
        envPath      = wsRoot </> ".env"
    hasLocal <- doesFileExist localEnvPath
    hasEnv   <- doesFileExist envPath
    raw <- if hasLocal
      then TIO.readFile localEnvPath
      else if hasEnv
        then TIO.readFile envPath
        else pure ""
    pure $ Map.fromList (parseDotEnv raw)

  let lookupVar name = do
        mEnv <- lookupEnv name
        pure $ case mEnv of
          Just v | not (null v) -> Just v
          _                     -> Map.lookup name dotEnvMap

  -- Resolve {ENV:VAR} placeholders in mergedCfg fields
  cfgKeyResolved  <- resolveEnvTemplates lookupVar (apiKey mergedCfg)
  cfgUrlResolved  <- resolveEnvTemplates lookupVar (apiBaseUrl mergedCfg)
  cfgModResolved  <- resolveEnvTemplates lookupVar (modelName mergedCfg)
  cfgHdrsResolved <- mapM (resolveEnvTemplates lookupVar) (customHeaders mergedCfg)

  mExplicitLambdaKey <- lookupVar "LAMBDA_API_KEY"
  mFallbackKey <- lookupVar "OPENROUTER_API_KEY" >>= \case
    Just k | not (null k) -> pure (Just k)
    _ -> lookupVar "OPENAI_API_KEY"

  let finalKey = case mExplicitLambdaKey of
        Just k | not (null k)           -> T.pack k
        _ | not (T.null cfgKeyResolved) -> cfgKeyResolved
        _                               -> maybe "" T.pack mFallbackKey

  mEnvUrl <- lookupVar "LAMBDA_BASE_URL" >>= \case
    Just u | not (null u) -> pure (Just u)
    _ -> lookupVar "OPENAI_BASE_URL"

  let finalUrl = maybe cfgUrlResolved T.pack mEnvUrl

  mEnvMod <- lookupVar "LAMBDA_MODEL" >>= \case
    Just m | not (null m) -> pure (Just m)
    _ -> lookupVar "OPENROUTER_MODEL" >>= \case
      Just m | not (null m) -> pure (Just m)
      _ -> lookupVar "OPENAI_MODEL"

  let finalMod = maybe cfgModResolved T.pack mEnvMod

  mEnvLimit <- lookupVar "CONTEXT_LIMIT"
  let finalLimit = case mEnvLimit of
        Just l | [(n, "")] <- reads l, n > 0 -> n
        _ -> contextWindowLimit mergedCfg

  mEnvTheme <- lookupVar "LAMBDA_THEME"
  let finalTheme = case mEnvTheme of
        Just t | not (null t) -> T.strip (T.pack t)
        _                     -> themeName mergedCfg

  let artDir = wsRoot </> ".lambda" </> "artifacts"

  createDirectoryIfMissing True artDir `catch` (\(_ :: IOException) -> pure ())
  createDirectoryIfMissing True (home </> ".config" </> "lambdA") `catch` (\(_ :: IOException) -> pure ())

  pure mergedCfg
    { apiBaseUrl         = finalUrl
    , apiKey             = finalKey
    , modelName          = finalMod
    , customHeaders      = cfgHdrsResolved
    , workspaceRoot      = wsRoot
    , artifactDir        = artDir
    , contextWindowLimit = finalLimit
    , themeName          = finalTheme
    }

-- | Resolve "{ENV:VAR_NAME}" or "{env:VAR_NAME}" placeholders using a lookup function
resolveEnvTemplates :: (String -> IO (Maybe String)) -> Text -> IO Text
resolveEnvTemplates lookupFn txt
  | "{ENV:" `T.isInfixOf` txt || "{env:" `T.isInfixOf` txt = do
      let (before, rest) = case T.breakOn "{ENV:" txt of
            (b, r) | not (T.null r) -> (b, r)
            _                       -> T.breakOn "{env:" txt
      if T.null rest
        then pure txt
        else do
          let afterPrefix = T.drop 5 rest -- drops "{ENV:" or "{env:"
              (varName, afterClose) = T.breakOn "}" afterPrefix
          if T.null afterClose
            then pure txt
            else do
              let cleanVar = T.unpack (T.strip varName)
              mVal <- lookupFn cleanVar
              let resolvedVal = maybe "" T.pack mVal
                  remainder = T.drop 1 afterClose -- drops "}"
              restResolved <- resolveEnvTemplates lookupFn remainder
              pure (before <> resolvedVal <> restResolved)
  | otherwise = pure txt

-- | Simple parser for KEY=VALUE pairs in .env / .env.local files
parseDotEnv :: Text -> [(String, String)]
parseDotEnv raw =
  [ (T.unpack (T.strip k), stripQuotes (T.unpack (T.strip (T.drop 1 rest))))
  | line <- T.lines raw
  , let trimmed = T.strip line
  , not (T.null trimmed)
  , not ("#" `T.isPrefixOf` trimmed)
  , let withoutExport = if "export " `T.isPrefixOf` trimmed then T.drop 7 trimmed else trimmed
  , let (k, rest) = T.breakOn "=" withoutExport
  , not (T.null rest)
  ]
  where
    stripQuotes s = case s of
      ('"':xs) | not (null xs) && last xs == '"' -> init xs
      ('\'':xs) | not (null xs) && last xs == '\'' -> init xs
      _ -> s

-- | Curated model aliases mapping friendly shorthands to full provider model IDs
defaultModelAliases :: Map Text Text
defaultModelAliases = Map.fromList
  [ ("claude",     "anthropic/claude-3.5-sonnet")
  , ("sonnet",     "anthropic/claude-3.5-sonnet")
  , ("claude-3.7", "anthropic/claude-3.7-sonnet")
  , ("r1",         "deepseek/deepseek-r1")
  , ("deepseek",   "deepseek/deepseek-r1")
  , ("4o",         "openai/gpt-4o")
  , ("gpt4",       "openai/gpt-4o")
  , ("o3",         "openai/o3-mini")
  , ("qwen",       "qwen/qwen-2.5-coder-32b-instruct")
  , ("coder",      "qwen/qwen-2.5-coder-32b-instruct")
  ]

-- | Resolve an alias or model name to its canonical identifier
resolveModelAlias :: Text -> Text
resolveModelAlias rawName =
  let clean = T.strip (T.toLower rawName)
  in Map.findWithDefault rawName clean defaultModelAliases

-- | Context window limits for known models (defaults to 128000)
lookupModelContextLimit :: Text -> Int
lookupModelContextLimit modId
  | "claude" `T.isInfixOf` modId   = 200000
  | "o3-mini" `T.isInfixOf` modId  = 200000
  | "gpt-4o" `T.isInfixOf` modId   = 128000
  | "deepseek" `T.isInfixOf` modId = 128000
  | "qwen" `T.isInfixOf` modId     = 128000
  | otherwise                      = 128000

-- | Check if api_base_url points to a local provider (Ollama, vLLM, LM Studio, etc.)
isLocalEndpoint :: Text -> Bool
isLocalEndpoint url =
  let u = T.toLower url
  in "localhost" `T.isInfixOf` u
     || "127.0.0.1" `T.isInfixOf` u
     || "0.0.0.0" `T.isInfixOf` u
     || "192.168." `T.isInfixOf` u
     || "10." `T.isInfixOf` u
     || ":11434" `T.isInfixOf` u
     || ":8000" `T.isInfixOf` u
     || ":1234" `T.isInfixOf` u
     || ":8080" `T.isInfixOf` u

-- | Check if api_base_url is direct OpenAI API
isOpenAiEndpoint :: Text -> Bool
isOpenAiEndpoint url =
  let u = T.toLower url
  in "api.openai.com" `T.isInfixOf` u

-- | Check if api_base_url is OpenRouter
isOpenRouterEndpoint :: Text -> Bool
isOpenRouterEndpoint url =
  let u = T.toLower url
  in "openrouter.ai" `T.isInfixOf` u

-- | Return a concise badge classifying provider topology.
-- Local endpoints are explicitly badged with "[local] " so local execution is unambiguous.
-- Remote endpoints leave the badge empty to avoid cluttering the prompt with redundant tags.
formatEndpointBadge :: Text -> Text
formatEndpointBadge url
  | isLocalEndpoint url = "[local] "
  | otherwise           = ""

-- | Derive human-readable provider name from API base URL
deriveProviderFromUrl :: Text -> Text
deriveProviderFromUrl url
  | T.null url                  = "none"
  | isOpenRouterEndpoint url    = "openrouter"
  | isOpenAiEndpoint url        = "openai"
  | isLocalEndpoint url         = "local"
  | "anthropic" `T.isInfixOf` u = "anthropic"
  | "groq" `T.isInfixOf` u       = "groq"
  | "deepseek" `T.isInfixOf` u   = "deepseek"
  | "together" `T.isInfixOf` u   = "together"
  | otherwise =
      let stripProto = T.replace "https://" "" (T.replace "http://" "" url)
          host = T.takeWhile (\c -> c /= '/' && c /= ':') stripProto
          parts = T.splitOn "." host
      in case filter (\p -> p /= "api" && p /= "com" && p /= "ai" && p /= "io" && p /= "org") parts of
           (p:_) -> p
           _     -> if T.null host then "custom" else host
  where
    u = T.toLower url

-- | Extract separate provider and model names from endpoint URL and configured model string
extractProviderAndModel :: Text -> Text -> (Text, Text)
extractProviderAndModel baseUrl rawModel
  | T.null baseUrl && T.null rawModel = ("none", "none")
  | "/" `T.isInfixOf` rawModel =
      let (p, mWithSlash) = T.breakOn "/" rawModel
          m = T.drop 1 mWithSlash
          prov = if T.null p then deriveProviderFromUrl baseUrl else p
          modl = if T.null m then "none" else m
      in (prov, modl)
  | otherwise =
      (deriveProviderFromUrl baseUrl, if T.null rawModel then "none" else rawModel)

-- | Resolve model alias taking active Config into account (user aliases, endpoint type)
resolveModelWithConfig :: Config -> Text -> Text
resolveModelWithConfig cfg rawName =
  let clean = T.strip rawName
      cleanLower = T.toLower clean
      userAliases = modelAliases cfg
      mUserMatch = Map.lookup clean userAliases
               <|> Map.lookup cleanLower userAliases
  in case mUserMatch of
       Just resolved -> resolved
       Nothing
         -- When unconfigured: pass through raw input without inventing cloud mappings
         | T.null (apiBaseUrl cfg) -> clean
         -- Direct OpenAI endpoint: use clean IDs without OpenRouter vendor prefixes
         | isOpenAiEndpoint (apiBaseUrl cfg) ->
             case cleanLower of
               "4o"       -> "gpt-4o"
               "gpt-4o"   -> "gpt-4o"
               "4o-mini"  -> "gpt-4o-mini"
               "gpt4"     -> "gpt-4o"
               "o3"       -> "o3-mini"
               "o3-mini"  -> "o3-mini"
               "o1"       -> "o1"
               "o1-mini"  -> "o1-mini"
               _          -> clean
         -- Local endpoints (Ollama, vLLM, LM Studio): never inject cloud prefixes
         | isLocalEndpoint (apiBaseUrl cfg) ->
             case filter (\m -> T.toLower m == cleanLower) (configuredModels cfg) of
               (matched:_) -> matched
               []          -> clean
         -- Explicit OpenRouter endpoint: use curated OpenRouter map
         | isOpenRouterEndpoint (apiBaseUrl cfg) ->
             Map.findWithDefault clean cleanLower defaultModelAliases
         -- Other generic remote endpoints
         | otherwise -> clean

-- | Curated list of popular models for auto-completion
curatedModels :: [Text]
curatedModels =
  [ "anthropic/claude-3.5-sonnet"
  , "anthropic/claude-3.7-sonnet"
  , "deepseek/deepseek-r1"
  , "openai/gpt-4o"
  , "openai/o3-mini"
  , "qwen/qwen-2.5-coder-32b-instruct"
  ]

-- | Surgically append an approved glob pattern to always_allow_globs in <wsRoot>/.lambda/config.json
persistAllowGlob :: FilePath -> String -> IO (Either Text ())
persistAllowGlob wsRoot newGlob = do
  let dirPath  = wsRoot </> ".lambda"
      confPath = dirPath </> "config.json"
  createDirectoryIfMissing True dirPath
  exists <- doesFileExist confPath
  baseObj <- if exists
    then do
      content <- BS.readFile confPath
      pure $ case Aeson.decodeStrict content of
        Just (Aeson.Object o) -> o
        _                     -> KM.empty
    else pure KM.empty

  let currentGlobs = case KM.lookup (Key.fromString "always_allow_globs") baseObj of
        Just (Aeson.Array arr) ->
          [ T.unpack s | Aeson.String s <- V.toList arr ]
        _ -> defaultAllowGlobs

  if newGlob `elem` currentGlobs
    then pure (Right ())
    else do
      let updatedGlobs = currentGlobs ++ [newGlob]
          newObj = KM.insert (Key.fromString "always_allow_globs") (Aeson.toJSON updatedGlobs) baseObj
          encoded = Aeson.encode newObj
      BL.writeFile confPath encoded
      pure (Right ())

-- | Surgically persist the active theme name to <wsRoot>/.lambda/config.json
persistTheme :: FilePath -> Text -> IO (Either Text ())
persistTheme wsRoot newTheme = do
  let dirPath  = wsRoot </> ".lambda"
      confPath = dirPath </> "config.json"
  createDirectoryIfMissing True dirPath
  exists <- doesFileExist confPath
  !baseObj <- if exists
    then do
      content <- BS.readFile confPath
      pure $ case Aeson.decodeStrict content of
        Just (Aeson.Object o) -> o
        _                     -> KM.empty
    else pure KM.empty
  let newObj = KM.insert (Key.fromString "theme") (Aeson.toJSON newTheme) baseObj
  BL.writeFile confPath (Aeson.encode newObj)
  pure (Right ())
