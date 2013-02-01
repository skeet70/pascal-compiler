-- Authored by: Tyler J. Huffman, Ryan "Murph" Murphy
-- Date: 1/24/2013
-- Adapted from http://learnyouahaskell.com/input-and-output
--
-- This file takes in one argument and throws away the rest.
-- It's also extensible enough to allow for more arguments by
-- modifying the pattern-matching at the start of the "do"
-- function
--
-- The driver function then reads the file into a string, and calls
-- getToken with the default values.

import System.Environment
import System.IO
import System.Directory
import Prelude hiding (catch)
import Control.Exception
import Scanner
import TokenTable

main = driver `catch` inputError

driver :: IO ()
driver = do
    (filename:_) <- getArgs
    source <- readFile filename
    if ((dropWhile (/= '.') filename) == ".mp")
    then extractData $ getToken (source, lexeme, column, line)
    else putStrLn "Please insert a valid file."
      where
        lexeme = ""
        column = 0
        line = 0

extractData :: (String, String, Token, Int, Int) -> IO ()
extractData (source, lexeme, token, column, line) = do
    if token == EndOfFile MP_EOF
    then putStrLn ("End of File")
    else putStrLn (unwrapToken token ++ " " ++ show line ++ " " ++ show column ++ " " ++ lexeme)
    --extractData $ getToken (source, lexeme, column, line)

inputError :: IOError -> IO ()
inputError e = putStrLn "Please insert a valid file."
