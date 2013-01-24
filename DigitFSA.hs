--Authored by Tyler J. Huffman
--
--This is the list of functions used in the microPascal compiler
module DigitFSA where
import Data.List
import Data.Char


digitFSA :: (String, String, Int, Int) -> (String, String, Int, Int)
digitFSA (src, lexeme, column_number, line_number)
    | stringHead == ['0'..'9']
        = digitFSA          (tail src, lexeme ++ stringHead, column_number + 1, line_number)
    | stringHead == "e" || stringHead == "E" 
        = digitFSAforE      (tail src, lexeme ++ stringHead, column_number + 1, line_number)
    | stringHead == "."                     
        = digitFSAforPeriod (tail src, lexeme ++ stringHead, column_number + 1, line_number)
    | otherwise                             
        = (src, lexeme, column_number, line_number)
    where stringHead = show ( head src)

digitFSAforE :: (String, String, Int, Int) -> (String, String, Int, Int)
digitFSAforE (src, lexeme, column_number, line_number) = (src, lexeme, column_number, line_number)
--    | stringHead

digitFSAforPeriod :: (String, String, Int, Int) -> (String, String, Int, Int)
digitFSAforPeriod (src, lexeme, column_number, line_number) = (src, lexeme, column_number, line_number)

