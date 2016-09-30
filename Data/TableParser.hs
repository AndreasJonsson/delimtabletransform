{- |
Module                   : Data.Tableparser
Description              : Parse a table according to a configuration.
Copyright                : (c) Andreas Jonsson 2016, Kreablo AB
License                  : GPL-3
Maintainer               : andreas.jonsson@kreablo.se
Stability                : experimental

Parse a file on a delimited format to produce a data structure representing a table.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.TableParser(
  transformTable,
  TableRow(..),
  TableCell(..)
) where

import Prelude(Show, (-), ($), (++), String, foldr)
import Data.Attoparsec.Text.Lazy
import Data.TransformConfig
import Control.Applicative
import Data.Text               (Text, cons)
import Data.Monoid
import System.IO                                    hiding (hGetContents)
import Control.Monad.Error     (strMsg, throwError)
import Control.Monad
import Data.Text.Lazy.IO       (hGetContents)
import Data.Maybe

newtype TableRow = TableRow [TableCell]         deriving Show
newtype TableCell = TableCell (Maybe Text)      deriving Show

-- | Read input and transform the data to table rows.  The table rows
--   are passed to a callback function in order.
transformTable :: TransformConfig           -- | Configuration for the parsing.
               -> [String]                  -- | List of filenames.
               -> (TableRow -> IO ())       -- | Callback function
               -> IO ()
transformTable conf files cb = mapM_ doFile files
    where
      doFile file = withFile file ReadMode
                             (\h -> do
                                hSetEncoding h (encoding conf)
                                hGetContents h >>= parseTable)

      parseTable ""       = pure ()
      parseTable contents = case parse (row conf) contents of
                              Done contents' r -> do
                                cb r
                                parseTable contents'
                              Fail _ _ msg      -> throwError $ strMsg msg

row ::  TransformConfig -> Parser TableRow
row c = (\a b -> TableRow $ a <> pure b) <$> count (numColumns c - 1) (column c) <*> lastColumn c

column :: TransformConfig -> Parser TableCell
column c = TableCell <$> columnText (fmap endTok $ columnDelimiters c) (substitutions c)

lastColumn :: TransformConfig -> Parser TableCell
lastColumn c = TableCell <$> columnText (fmap endTok (rowDelimiters c) ++ [endOfInput *> pure ""]) (substitutions c)

columnText :: [Parser Text] -> [(Text, Text)] -> Parser (Maybe Text)
columnText stopTokens substs = checkNull <$> text
    where
      text :: Parser Text
      text = foldr (\t -> (t <|>)) moreText (substText substs ++ stopTokens)
      substText    :: [(Text, Text)] -> [Parser Text]
      substText = fmap (\s -> substitution s `cat` text)
      moreText     :: Parser Text
      moreText     = cons <$> anyChar <*> text
      checkNull :: Text -> (Maybe Text)
      checkNull t = case t of
                      "\0" -> Nothing
                      _ -> Just t


cat :: Parser Text -> Parser Text -> Parser Text
cat t1 t2 = (<>) <$> t1 <*> t2

endTok :: Text -> Parser Text
endTok t = string t *> pure ""

substitution :: (Text, Text) -> Parser Text
substitution (from, to) = string from *> pure to