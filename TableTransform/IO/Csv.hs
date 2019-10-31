{-# LANGUAGE OverloadedStrings #-}

module TableTransform.IO.Csv (
  printRow
) where

import Prelude hiding(putStr)
import Data.Text(Text)
import Data.Text.IO(putStr)
import TableTransform.Text.TableParser(TableRow(..), TableCell(..))

printRow :: Text -> Text -> Text -> TableRow -> IO ()
printRow enclose columnDelim rowDelim (TableRow cells) = do
  printCells enclose columnDelim cells
  putStr rowDelim

printCells :: Text -> Text -> [TableCell] -> IO ()
printCells _ _ [] = pure ()
printCells enclose _ [tc] = printCellText enclose tc
printCells enclose columnDelim (tc:cells) = do
  printCellText enclose tc
  putStr columnDelim
  printCells enclose columnDelim cells

printCellText :: Text -> TableCell -> IO ()
printCellText enclose (TableCell (Just t)) = do
  putStr enclose
  putStr t
  putStr enclose
printCellText _       (TableCell Nothing)  = putStr ""
