module Data.TransformConfig(
  TransformConfig(..)
) where

import Prelude       (Int)
import System.IO     (TextEncoding)
import Data.Text     (Text)

data TransformConfig = TransformConfig {
      columnDelimiters   :: [Text],
      rowDelimiters      :: [Text],
      outputRowDelimiter :: Text,
      numColumns         :: Int,
      encodingText       :: Text,
      encoding           :: TextEncoding
    }

