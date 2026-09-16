{-# LANGUAGE OverloadedStrings #-}

module Lambda.Engine.PromptMacro
  ( listPromptMacros
  , loadPromptMacro
  , expandPromptMacro
  ) where

import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension, dropExtension)

-- | List available prompt macro names in `<wsRoot>/.lambda/prompts/`
listPromptMacros :: FilePath -> IO [Text]
listPromptMacros wsRoot = do
  let promptDir = wsRoot </> ".lambda" </> "prompts"
  exists <- doesDirectoryExist promptDir
  if not exists
    then pure []
    else do
      files <- listDirectory promptDir
      pure [ T.pack (dropExtension f)
           | f <- files
           , takeExtension f == ".md"
           ]

-- | Load and expand a prompt macro with user arguments
loadPromptMacro :: FilePath -> Text -> Text -> IO (Either Text Text)
loadPromptMacro wsRoot name args = do
  let promptPath = wsRoot </> ".lambda" </> "prompts" </> T.unpack name <> ".md"
  res <- try (TIO.readFile promptPath) :: IO (Either SomeException Text)
  case res of
    Left _ -> pure $ Left ("Prompt macro not found: " <> name <> " (expected at .lambda/prompts/" <> name <> ".md)")
    Right rawTemplate -> pure $ Right (expandPromptMacro rawTemplate args)

-- | Substitute arguments into template:
-- If template contains $input, replace all occurrences of $input with args.
-- If $input is absent and args is non-empty, append args to the prompt.
expandPromptMacro :: Text -> Text -> Text
expandPromptMacro template args
  | "$input" `T.isInfixOf` template =
      T.replace "$input" (T.strip args) template
  | "$ARG" `T.isInfixOf` template =
      T.replace "$ARG" (T.strip args) template
  | not (T.null (T.strip args)) =
      T.stripEnd template <> "\n\n" <> T.strip args
  | otherwise =
      template
