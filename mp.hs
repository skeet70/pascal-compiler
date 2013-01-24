-- Authored by: Tyler J. Huffman, Ryan "Murph" Murphy
-- Date: 1/24/2013
-- Adapted from http://learnyouahaskell.com/input-and-output
--
-- This file takes in one argument and throws away the rest.
-- It's also extensible enough to allow for more arguments by
-- modifying the pattern-matching at the start of the "do"
-- function
--
-- The toTry function then reads the file into a string, and calls
-- getToken with the default values.

import System.Environment
import System.IO
import System.Directory
import Prelude hiding (catch)
import Control.Exception

import Scanner

main = toTry `catch` handler

toTry :: IO ()
toTry = do (filename:_) <- getArgs
            if (dropWhile (/= '.') filename) == ".mp"
                then getNextToken (source, lexeme, columnNumber, lineNumber)
                  where
                    source = read filename
                    lexeme = ""
                    columnNumber = 0
                    lineNumber = 0
                else putStrLn "Please insert a valid file."

handler :: IOError -> IO ()
handler e = putStrLn "Please insert a valid file."