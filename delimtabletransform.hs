{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Prelude                    (flip, id, ($), (++), concat, String, foldl, read, Int, (<=), (.), show)
import Data.Text                  (pack, unpack)
import System.Console.GetOpt
import Control.Applicative
import Control.Arrow
import Control.Monad
import System.Environment         (getArgs)
import System.IO
import System.IO.Error
import Control.Monad.Error
import Data.Monoid
import Data.TransformConfig
import Data.TableParser

defaultOptions :: TransformConfig
defaultOptions = TransformConfig {
                   columnDelimiters   = [],
                   rowDelimiters      = [],
                   outputRowDelimiter = "!!!\\x1e\\n",
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
 Option [] ["encoding"]         (ReqArg (\d opts -> opts {     encodingText = pack d }) "ENCODING")
         "Character encoding of the input files.",
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
validateAndFillDefault = nc >> (cd >>> rd >>> od >>> enc)
  where
    cd c = case columnDelimiters c of
             [] -> c { columnDelimiters = [ "!*!" ] }
             _  -> c { columnDelimiters = esc $ columnDelimiters c }
    rd c = case rowDelimiters c of
             [] -> c { rowDelimiters    = [ "\n", "\r\n" ] }
             _  -> c { rowDelimiters    = esc $ rowDelimiters c }
    nc :: TransformConfig -> IO ()
    nc c = when (numColumns c <= 0) $ throwError $ strMsg "--num-columns must be set!"
    od c = c { outputRowDelimiter = esc' $ outputRowDelimiter c }
    enc c = case encodingText c of
              "utf8"  -> pure c { encoding = utf8  }
              "utf16" -> pure c { encoding = utf16 }
              e       -> throwError $ strMsg $ "Invalid character encoding: '" ++ unpack e ++ "'"
    esc = fmap esc'
    esc' = (\s -> "\"" <> s <> "\"") >>> unpack >>> read

main :: IO ()
main = do
  (config, files) <- getArgs >>= getOptions >>= runKleisli (first $ Kleisli validateAndFillDefault)
  transformTable config files dumpRow

dumpRow :: TableRow -> IO ()
dumpRow =  print . show