{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Artifacts
  ( spoolDiagnosticArtifact
  , spooDiagnosticArtifact
  , formatArtifactPointer
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | Spools raw output out-of-band if it exceeds the threshold (500 chars or 40 lines).
-- Returns a compact summary string along with the optional artifact file path.
spoolDiagnosticArtifact :: FilePath -> Text -> Text -> IO (Text, Maybe FilePath)
spoolDiagnosticArtifact artifactDir toolName rawOutput = do
  let lineCount = length (T.lines rawOutput)
      charCount = T.length rawOutput

  if charCount > 500 || lineCount > 40
    then do
      createDirectoryIfMissing True artifactDir
      now <- getCurrentTime
      let ts = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
          pico = take 6 (formatTime defaultTimeLocale "%q" now)
          filename = T.unpack toolName <> "_" <> ts <> "_" <> pico <> ".log"
          fullPath = artifactDir </> filename
      BS.writeFile fullPath (TE.encodeUtf8 rawOutput)

      -- Generate summary window without negative counts or overlapping head/tail lines
      let ls = T.lines rawOutput
          summaryText
            | lineCount > 40 =
                let headLines = take 15 ls
                    tailLines = drop (max 0 (lineCount - 25)) ls
                in T.unlines
                     [ T.unlines headLines
                     , "... [" <> T.pack (show (lineCount - 40)) <> " lines omitted. Full dump spooled out-of-band] ..."
                     , T.unlines tailLines
                     ]
            | otherwise =
                T.unlines
                  [ "... [" <> T.pack (show (max 0 (charCount - 500))) <> " chars omitted. Full dump spooled out-of-band] ..."
                  , T.takeEnd 500 rawOutput
                  ]
          pointer = formatArtifactPointer (T.pack filename) fullPath summaryText lineCount
      pure (pointer, Just fullPath)
    else
      pure (rawOutput, Nothing)

-- | Formats an invariant artifact pointer handle for prompt ingestion
formatArtifactPointer :: Text -> FilePath -> Text -> Int -> Text
formatArtifactPointer artId path synopsis totalLines =
  T.unlines
    [ "<artifact_pointer id=\"" <> artId <> "\" path=\"" <> T.pack path <> "\" total_lines=\"" <> T.pack (show totalLines) <> "\">"
    , synopsis
    , "</artifact_pointer>"
    ]

-- | Backward-compatible alias for typo in earlier versions
spooDiagnosticArtifact :: FilePath -> Text -> Text -> IO (Text, Maybe FilePath)
spooDiagnosticArtifact = spoolDiagnosticArtifact

