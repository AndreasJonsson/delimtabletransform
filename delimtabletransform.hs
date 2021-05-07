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

import Prelude                    (flip, id, ($), (++), concat, String, foldl, read, Bool(..))
import Data.Text                  (pack, unpack, Text)
import System.Console.GetOpt
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment         (getArgs)
import System.IO
import System.IO.Error
import Data.Monoid
import TableTransform.Config.TransformConfig
import TableTransform.Text.TableParser
import TableTransform.IO.Csv
import Data.List
import Data.Maybe

defaultOptions :: TransformConfig
defaultOptions = TransformConfig {
                   columnDelimiters   = [],
                   rowDelimiters      = [],
                   substitutions      = [],
                   enclosedBy         = "\"",
                   escapedQuote       = "\"\"",
                   inputEnclosedBy    = "\"",
                   inputEscapedQuote  = "\"\"",
                   outputColumnDelimiter = ",",
                   outputRowDelimiter = "\\r\\n",
                   numColumns         = Nothing,
                   encodingText       = "utf8",
                   encoding           = utf8,
                   nullLiteral        = False,
                   headerRows         = 0
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
 Option [] ["escaped-quote"] (OptArg (\d opts -> opts    { escapedQuote     = fromMaybe "" $ pack <$> d }) "ESCAPE")
         "esqaped quote character sequence for the input",
 Option [] ["num-columns"]      (OptArg (\d opts -> opts {       numColumns = case d of
                                                                                Just d' -> Just (read d')
                                                                                Nothing -> Nothing
                                                         }) "COLUMNS")
         "Number of columns in the input.",
 Option [] ["input-enclosed-by"] (OptArg (\d opts -> opts { inputEnclosedBy  = fromMaybe "" $ pack <$> d }) "TEXT")
         "start quote character sequence for the input",
 Option [] ["input-escaped-quote"] (OptArg (\d opts -> opts { inputEscapedQuote      = fromMaybe "" $ pack <$> d }) "ESCAPE")
         "esqaped quote character sequence for the input",
 Option [] ["output-column-delimiter"] (ReqArg(\d opts -> opts { outputColumnDelimiter = pack d }) "DELIMITER" )
         "Output row delimiter (default '\\r\\n')",
 Option [] ["output-row-delimiter"] (ReqArg(\d opts -> opts { outputRowDelimiter = pack d }) "DELIMITER" )
         "Output row delimiter (default '\\r\\n')",
 Option [] ["null-literal"] (NoArg (\opts -> opts { nullLiteral = True }))
         "Recognize the literal NULL as NULL value.",
 Option [] ["header-rows"]      (ReqArg (\d opts -> opts {       headerRows = read d }) "NUMBER")
         "Number of header rows to skip."]

getOptions :: [String] -> IO (TransformConfig, [String])
getOptions argv =
    case getOpt RequireOrder options argv of
      (o, n, []) -> pure (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: delimtabletransform [OPTION...] files..."

validateAndFillDefault :: TransformConfig -> IO TransformConfig
validateAndFillDefault = cd >>> rd >>> od >>> addSubst >>> enc
  where
    cd :: TransformConfig -> TransformConfig
    cd c = case columnDelimiters c of
             [] -> c { columnDelimiters = [ "!*!" ] }
             _  -> c { columnDelimiters = esc $ columnDelimiters c }
    rd :: TransformConfig -> TransformConfig
    rd c = case rowDelimiters c of
             [] -> c { rowDelimiters    = [ "\n", "\r\n" ] }
             _  -> c { rowDelimiters    = esc $ rowDelimiters c }
    od :: TransformConfig -> TransformConfig
    od c = c { outputRowDelimiter = esc' $ outputRowDelimiter c }
    enc :: TransformConfig -> IO TransformConfig
    enc c = case encodingText c of
              "utf8"   -> pure c { encoding = utf8_bom  }
              "utf16"  -> pure c { encoding = utf16 }
              "latin1" -> pure c { encoding = latin1 }
              "iso-8859-1" -> pure c { encoding = latin1 }
              e       -> fail $ "Invalid character encoding: '" ++ unpack e ++ "'"
    esc :: [Text] -> [Text]
    esc = fmap esc'
    esc' :: Text -> Text
    esc' = (\s -> "\"" <> s <> "\"") >>> unpack >>> read
    addSubst :: TransformConfig -> TransformConfig
    addSubst c = c { substitutions = substitutions c ++ [(enclosedBy c, "\\" <> enclosedBy c),
                                                         ("\\"        , "\\\\"              ) ] }

parseSubst :: String -> [(Text, Text)]
parseSubst s = case elemIndex '=' s of
                 Just i  -> pure $ (splitAt i >>> second (drop 1) >>> pack *** pack) s
                 Nothing -> []

main :: IO ()
main = do
  (config, files) <- getArgs >>= getOptions >>= runKleisli (first $ Kleisli validateAndFillDefault)
  transformTable config files (printRow (enclosedBy config) 
                                (outputColumnDelimiter config)
                                (outputRowDelimiter config))

