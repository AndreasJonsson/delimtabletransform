{- |
Module                   : Data.TransformConfig
Description              : Configuration for parsing table data
Copyright                : (c) Andreas Jonsson 2016, Kreablo AB
License                  : GPL-3
Maintainer               : andreas.jonsson@kreablo.se
Stability                : experimental
-}
module Data.TransformConfig(
  TransformConfig(..)
) where

import Prelude       (Int, Show, Bool)
import System.IO     (TextEncoding)
import Data.Text     (Text)

data TransformConfig = TransformConfig {
      -- | The column delimiters verbatim text that terminates
      --   columns.  Any one of the delimiters terminates the column.
      columnDelimiters   :: [Text],
      -- | The row delimiters verbatim text that terminates rows.
      --   Any one of the delimiters terminates the row.
      rowDelimiters      :: [Text],
      -- | The first member of the tuple is verbatim text that should
      --   be substituted with the text in the second member.
      substitutions      :: [(Text, Text)],
      -- | The expected number of columns in the input.  'numColumns' is a hint used by the parsing.
      numColumns         :: Int,
      -- | The transformed output will be enclosed by the 'enclosedBy' text fragment.
      enclosedBy         :: Text,
      -- | The transformed output will used the 'outputRowDeliter' text fragment.
      outputRowDelimiter :: Text,
      -- | The character encoding of the input.  (Text obtained from command line parameter.)
      encodingText       :: Text,
      -- | The character encoding of the input.
      encoding           :: TextEncoding,
      -- | Recognize the literal string NULL as null value.
      nullLiteral        :: Bool
    } deriving Show

