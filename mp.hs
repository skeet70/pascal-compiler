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
    else putStrLn "Please insert a valid file."
      where
        lexeme = ""
        column = 1
        line = 1
        parsingData = ParsingData {input=[]}

scanFile :: (String, String, Token, Int, Int) -> ParsingData -> ParsingData
scanFile (source, lexeme, token, column, line) parsingData
    | token == EndOfFile MP_EOF
        = ParsingData {   lookAheadToken=token (head (input parsingData))
                        , line=line (head (input parsingData))
                        , column=column (head (input parsingData))
                        , input=input parsingData ++ scannerData
                    }
    | otherwise
        = scanFile (getToken (source, "", column, line)) newParsing
      where
        scannerData = convertToScannerData (source, lexeme, token, column, line)
        newParsing = ParsingData {input=input parsingData ++ scannerData}

parse :: ParsingData -> ParsingData
parse parsingData = systemGoal parsingData

convertToScannerData :: (String, String, Token, Int, Int) -> ScannerData
convertToScannerData (source, lexeme, token, column, line)
    = ScannerData {   token=token
                    , line=line
                    , column=column
                }

packParsingData :: ScannerData -> ParsingData -> ParsingData
packParsingData scannerData parsingData
    = ParsingData

extractData :: (String, String, Token, Int, Int) -> IO ()
extractData (source, lexeme, token, column, line) = do
    putStrLn (unwrapToken token ++ " " ++ show line ++ " " ++ show column ++ " " ++ lexeme)
    if token == EndOfFile MP_EOF
    then putStrLn "Done Scanning."
    else extractData $ getToken (source, "", column, line)

inputError :: IOError -> IO ()
inputError e = putStrLn "Please insert a valid file."
