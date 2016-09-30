{- |
Module                   : Main
Description              : Command line tool for transforming table formats.
Copyright                : (c) Andreas Jonsson 2016, Kreablo AB
License                  : GPL-3
Maintainer               : andreas.jonsson@kreablo.se
Stability                : experimental
-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Prelude                    (flip, id, ($), (++), concat, String, foldl, read, (<=), (==), (.))
import Data.Text                  (pack, unpack, Text)
import Data.Text.IO               (putStr)
import System.Console.GetOpt
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment         (getArgs)
import System.IO                  hiding (putStr)
import System.IO.Error
import Control.Monad.Error
import Data.Monoid
import Data.TransformConfig
import Data.TableParser
import Data.List
import Data.Maybe

defaultOptions :: TransformConfig
defaultOptions = TransformConfig {
                   columnDelimiters   = [],
                   rowDelimiters      = [],
                   substitutions      = [],
                   enclosedBy         = "\"",
                   outputRowDelimiter = "!#!\\r\\n",
                   numColumns         = 0,
                   encodingText       = "utf8",
                   encoding           = utf8
                 }

options :: [OptDescr (TransformConfig -> TransformConfig)]
options = [
 Option [] ["column-delimiter"] (ReqArg (\d opts -> opts { columnDelimiters = columnDelimiters opts ++ [pack d] }) "DELIMITER")
        "column delimiters (default ['!*!'])",
 Option [] ["row-delimiter"]    (ReqArg (\d opts -> opts {    rowDelimiters =    rowDelimiters opts ++ [pack d] }) "DELIMITER")
         "row delimiters (default ['\\n', '\\r\\n'])",
 Option [] ["substitution"]     (ReqArg (\d opts -> opts {    substitutions =    substitutions opts ++ parseSubst d }) "TEXT=TEXT")
         "substitution text on the form <from text>=<to text>",
 Option [] ["encoding"]         (ReqArg (\d opts -> opts {     encodingText = pack d }) "ENCODING")
         "Character encoding of the input files.",
 Option [] ["enclosed-by"]      (ReqArg (\d opts -> opts {       enclosedBy = pack d }) "TEXT")
         "Character sequence to enclose fields by",
 Option [] ["num-columns"]      (ReqArg (\d opts -> opts {       numColumns = read d }) "COLUMNS")
         "Number of columns in the input.",
 Option [] ["output-row-delimiter"] (ReqArg(\d opts -> opts { outputRowDelimiter = pack d }) "DELIMITER" )
         "Output row delimiter (default '!!!\\x1e\\n')"]

getOptions :: [String] -> IO (TransformConfig, [String])
getOptions argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> pure (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: delimtabletransform [OPTION...] files..."

validateAndFillDefault :: TransformConfig -> IO TransformConfig
validateAndFillDefault = cd >>> rd >>> od >>> addSubst >>> enc >=> nc
  where
    cd :: TransformConfig -> TransformConfig
    cd c = case columnDelimiters c of
             [] -> c { columnDelimiters = [ "!*!" ] }
             _  -> c { columnDelimiters = esc $ columnDelimiters c }
    rd :: TransformConfig -> TransformConfig
    rd c = case rowDelimiters c of
             [] -> c { rowDelimiters    = [ "\n", "\r\n" ] }
             _  -> c { rowDelimiters    = esc $ rowDelimiters c }
    nc :: TransformConfig -> IO TransformConfig
    nc c = do
      when (numColumns c <= 0) $ throwError $ strMsg "--num-columns must be set!"
      pure c
    od :: TransformConfig -> TransformConfig
    od c = c { outputRowDelimiter = esc' $ outputRowDelimiter c }
    enc :: TransformConfig -> IO TransformConfig
    enc c = case encodingText c of
              "utf8"   -> pure c { encoding = utf8  }
              "utf16"  -> pure c { encoding = utf16 }
              "latin1" -> pure c { encoding = latin1 }
              e       -> throwError $ strMsg $ "Invalid character encoding: '" ++ unpack e ++ "'"
    esc :: [Text] -> [Text]
    esc = fmap esc'
    esc' :: Text -> Text
    esc' = (\s -> "\"" <> s <> "\"") >>> unpack >>> read
    addSubst :: TransformConfig -> TransformConfig
    addSubst c = c { substitutions = substitutions c ++ [(enclosedBy c, "\\" <> enclosedBy c),
                                                         ("\\"        , "\\\\"              ) ] }

parseSubst :: String -> [(Text, Text)]
parseSubst s = case findIndex (== '=') s of
                 Just i  -> pure $ (splitAt i >>> second (drop 1) >>> pack *** pack) s
                 Nothing -> []

main :: IO ()
main = do
  (config, files) <- getArgs >>= getOptions >>= runKleisli (first $ Kleisli validateAndFillDefault)
  columnDelim <- case columnDelimiters config of
                   (cd:_) -> pure cd
                   _      -> throwError $ strMsg "No column delimiters!"
  transformTable config files (printRow (enclosedBy config) columnDelim $ outputRowDelimiter config)

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
printCellText _       (TableCell Nothing)  = putStr "NULL"