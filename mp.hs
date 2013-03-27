-- Authored by: Tyler J. Huffman, Ryan "Murph" Murphy
-- Date: 1/24/2013
-- Adapted from http://learnyouahaskell.com/input-and-output
--
-- This file takes in one argument and throws away the rest.
-- It's also extensible enough to allow for more arguments by
-- modifying the pattern-matching at the start of the "do"
-- function.
--
-- The driver function then reads the file into a string, and calls
-- getToken with the default values.

import Scanner.Dispatcher
import Scanner.TokenTable
import Scanner.ScannerData
import Parser.ParsingData
import Parser.ParsingFunctions (systemGoal)

import System.Environment
import System.IO
import System.Directory
import Prelude hiding (catch)
import Control.Exception

main = driver `catch` inputError

driver :: IO ()
driver = do
    (filename:_) <- getArgs
    source <- readFile filename
    if ((dropWhile (/= '.') filename) == ".mp")
    then parse (scanFile (getToken (source, lexeme, column, line)) parsingData)
    --then extractData $ getToken (source, lexeme, column, line)
    else putStrLn "Please insert a valid file."
      where
        lexeme = ""
        column = 1
        line = 1
        parsingData = ParsingData {hasFailed=False, input=[], symbolTables=[]}

scanFile :: (String, String, Token, Int, Int) -> ParsingData -> ParsingData
scanFile (source, lexeme, token_in, column_in, line_in) parsingData
    | token_in == EndOfFile MP_EOF
        = ParsingData {   hasFailed=False
                        , lookAheadToken=if null (input parsingData) then Symbol MP_SCOLON else token (head (input parsingData))
                        , line=if null (input parsingData) then 0 else line_scan (head (input parsingData))
                        , column=if null (input parsingData) then 0 else column_scan (head (input parsingData))
                        , input=input parsingData ++ [scannerData]
                        , symbolTables=[]
                    }
    | otherwise
        = scanFile (getToken (source, "", column_in, line_in)) newParsing
      where
        scannerData = convertToScannerData (source, lexeme, token_in, column_in, line_in)
        newParsing = ParsingData {hasFailed=False, input=input parsingData ++ [scannerData], symbolTables=[]}

parse :: ParsingData -> IO()
parse parsingData
    | hasFailed finalData == True
        = putStrLn ("Failed at column " ++ show (column finalData) ++ ", line " ++ show (line finalData) ++ " with message: " ++ errorString finalData)
    | otherwise
        = putStrLn (errorString finalData)
  where
    finalData = systemGoal parsingData

convertToScannerData :: (String, String, Token, Int, Int) -> ScannerData
convertToScannerData (source, lexeme, token, column, line)
    = ScannerData {   token=token
                    , line_scan=line
                    , column_scan=column
                }

packParsingData :: ScannerData -> ParsingData -> ParsingData
packParsingData scannerData parsingData
    = parsingData

extractData :: (String, String, Token, Int, Int) -> IO ()
extractData (source, lexeme, token, column, line) = do
    putStrLn (unwrapToken token ++ " " ++ show line ++ " " ++ show column ++ " " ++ lexeme)
    if token == EndOfFile MP_EOF
    then putStrLn "Done Scanning."
    else extractData $ getToken (source, "", column, line)

inputError :: IOError -> IO ()
inputError e = putStrLn "Please insert a valid file."
