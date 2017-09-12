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

import Prelude(Show, (-), ($), (++), String, foldr, Bool(..), Int, ($!), flip, (.), undefined)
import Data.Attoparsec.Text.Lazy
import Data.TransformConfig
import Control.Applicative
import Control.Arrow
import Data.Text               (Text, cons, unpack)
import Data.Monoid
import System.IO                                    hiding (hGetContents)
import Control.Monad.Error     (strMsg, throwError)
import Control.Monad
import Data.Text.Lazy.IO       (hGetContents)
import Data.Maybe

newtype TableRow = TableRow [TableCell]         deriving Show
newtype TableCell = TableCell (Maybe Text)      deriving Show

data EndType = EndOfCell | EndOfRow | EndOfInput deriving Show

{-
instance Monoid EndType where
  mempty = EndOfUnknown
  EndOfUnknown `mappend` e = e
  e `mappend` EndOfUnknown = e
  _ `mappend` _            = undefined
-}

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
                                hGetContents h >>= parseTableSkipHeader)


      parseTableSkipHeader content = do
        let Done contents' _ = parse (skipRows conf (headerRows conf)) content
        parseTable contents'
      
      parseTable ""       = pure ()
      parseTable contents =
        case parse (row conf) contents of
          Done contents' r -> do
            cb r
            parseTable contents'
          Fail _ _ msg      -> throwError $ strMsg msg

skipRows :: TransformConfig -> Int -> Parser ()
skipRows _ 0 = pure ()
skipRows c n = row c >> skipRows c (n - 1)

row ::  TransformConfig -> Parser TableRow
row c = case numColumns c of
  Just n -> combine (count (n - 1) (column c)) $ lastColumn c
  Nothing -> combine (many $ column c) $ lastColumn c
  where
    combine :: Parser [TableCell] -> Parser TableCell -> Parser TableRow
    combine p last = TableRow <$> ((flip (<>) <$> p) <*> (pure <$> last))

column :: TransformConfig -> Parser TableCell
column c = do
  (et, cell) <- genericCell c
  case et of
    EndOfCell -> pure cell
    _         -> mzero

lastColumn :: TransformConfig -> Parser TableCell
lastColumn c = do
  (et, cell) <- genericCell c
  case et of
    EndOfRow   -> pure cell
    EndOfInput -> pure cell
    EndOfCell  -> mzero

genericCell :: TransformConfig -> Parser (EndType, TableCell)
genericCell c = second TableCell <$> columnText c (eoc ++ eor ++ eoi) (substitutions c)
  where
    eoc :: [Parser EndType]
    eoc = fmap (endTok EndOfCell) (columnDelimiters c)
    eor :: [Parser EndType]
    eor = fmap (endTok EndOfRow) (rowDelimiters c)
    eoi :: [Parser EndType]
    eoi = [endOfInput *> pure EndOfInput]

columnText :: TransformConfig -> [Parser EndType] -> [(Text, Text)] -> Parser (EndType, Maybe Text)
columnText c stopTokens substs = checkNull <$> text
    where
      text :: Parser (EndType, Text)
      text = foldr (<|>) (moreText) (substText substs ++ (fmap (\e -> (e, "")) <$> stopTokens))
      substText    :: [(Text, Text)] -> [Parser (EndType, Text)]
      substText = fmap (\s -> substitution s `cat` text)
      moreText     :: Parser (EndType, Text)
      moreText     = second <$> (cons <$> anyChar) <*> text
      checkNull :: (EndType, Text) -> (EndType, Maybe Text)
      checkNull (e, t) = case (t, nullLiteral c) of
                           ("\0", _)      -> (e, Nothing)
                           ("NULL", True) -> (e, Nothing)
                           _              -> (e, Just t)


cat :: Parser Text -> Parser (EndType, Text) -> Parser (EndType, Text)
cat t1 t2 = second <$> ((<>) <$> t1) <*> t2

endTok :: EndType -> Text -> Parser EndType
endTok endType t = string t *> pure endType

substitution :: (Text, Text) -> Parser Text
substitution (from, to) = string from *> pure to
