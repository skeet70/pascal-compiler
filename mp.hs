--Authored by: Tyler J. Huffman
--Adapted from http://learnyouahaskell.com/input-and-output
--
--This file takes in one argument and throws away the rest.
--It's also extensible enough to allow for more arguments by
--modifying the pattern-matching at the start of the "do" 
--function
--
--Also, this is my first attempt, so I'll likely be modifying
--this as time goes on.

import System.Environment  
import System.IO  
import System.Directory  
  
main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           if (dropWhile (/= '.') fileName) == ".mp"
           then putStrLn $ "The file is named " ++ fileName
           else putStrLn "Please insert a valid file"

handler :: IOError -> IO ()  
handler e = putStrLn "Please insert a valid file"
