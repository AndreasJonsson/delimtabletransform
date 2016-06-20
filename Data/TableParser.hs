{-# LANGUAGE OverloadedStrings #-}

module Data.TableParser(
  transformTable,
  TableRow(..),
  TableColumn(..)
) where

import Prelude(Show, (-), ($), (++), String)
import Data.Attoparsec.Text.Lazy
import Data.TransformConfig
import Control.Applicative
import Data.Text               (Text, cons)
import Data.Monoid
import System.IO                                    hiding (hGetContents)
import Control.Monad.Error     (strMsg, throwError)
import Control.Exception       (bracket)
import Control.Monad
import Data.Text.Lazy.IO       (hGetContents)

newtype TableRow = TableRow [TableColumn] deriving Show
newtype TableColumn = TableColumn Text    deriving Show

transformTable :: TransformConfig -> [String] -> (TableRow -> IO ()) -> IO ()
transformTable conf files cb = mapM_ doFile files
    where
      doFile file = bracket (openFile file ReadMode)
                            (\h -> hClose h)
                            (\h -> do
                               hSetEncoding h (encoding conf)
                               hGetContents h >>= parseTable )

      parseTable contents = case parse (row conf) contents of
                              Done contents' r -> do
                                cb r
                                parseTable contents'
                              Fail _ _ msg      -> throwError $ strMsg msg

row ::  TransformConfig -> Parser TableRow
row c = TableRow <$> count (numColumns c - 1) (column c) <> (pure <$> lastColumn c)

column :: TransformConfig -> Parser TableColumn
column c = TableColumn <$> text (columnDelimiters c)

lastColumn :: TransformConfig -> Parser TableColumn
lastColumn c = TableColumn <$> text (rowDelimiters c)

text :: [Text] -> Parser Text
text stopTokens = text' stopTokens
    where
      text' :: [Text] -> Parser Text
      text' []     = cons <$> anyChar <*> text' stopTokens
      text' (t:ts) = (string t <|> endOfInput *> pure "") <|> text' ts
