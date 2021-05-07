{- |
Module                   : Data.TransformConfig
Description              : Configuration for parsing table data
Copyright                : (c) Andreas Jonsson 2016, Kreablo AB
License                  : GPL-3
Maintainer               : andreas.jonsson@kreablo.se
Stability                : experimental
-}
module TableTransform.Config.TransformConfig(
  TransformConfig(..)
) where

import Prelude       (Int, Show, Bool)
import System.IO     (TextEncoding)
import Data.Text     (Text)
import Data.Maybe

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
      numColumns         :: Maybe Int,
      -- | The transformed output will be enclosed by the 'enclosedBy' text fragment.
      enclosedBy         :: Text,
      -- | The escape sequence to use for an escaped enclose by character.
      escapedQuote       :: Text,
      -- | The transformed output will use the 'outputColumnDelimiter' text fragment to separate columns.
      inputEnclosedBy    :: Text,
      inputEscapedQuote  :: Text,
      outputColumnDelimiter :: Text,
      -- | The transformed output will use the 'outputRowDelimiter' text fragment to separate rows.
      outputRowDelimiter :: Text,
      -- | The character encoding of the input.  (Text obtained from command line parameter.)
      encodingText       :: Text,
      -- | The character encoding of the input.
      encoding           :: TextEncoding,
      -- | Recognize the literal string NULL as null value.
      nullLiteral        :: Bool,
      -- | Header rows to skip (default 0)
      headerRows         :: Int
    } deriving Show

