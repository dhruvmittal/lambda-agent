module Lambda.Core.ModelDriver
  ( ModelDriver(..)
  , simpleModelDriver
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Lambda.Types

-- | Abstract interface for LLM streaming completions and tool schemas
data ModelDriver = ModelDriver
  { streamCompletion
      :: [Turn]                     -- Sanitized conversation turns
      -> [Aeson.Value]               -- Available tool schemas
      -> (StreamChunk -> IO ())      -- Stream callback for chunks
      -> IO ()
  , driverWithModel :: Text -> ModelDriver
      -- ^ Pure derivation of a new driver scoped to a specific model ID,
      -- sharing the same underlying connection pool and credentials.
  }

-- | Helper to construct a ModelDriver with self-referential model derivation
simpleModelDriver :: ([Turn] -> [Aeson.Value] -> (StreamChunk -> IO ()) -> IO ()) -> ModelDriver
simpleModelDriver streamFn =
  let d = ModelDriver streamFn (\_ -> d)
  in d
