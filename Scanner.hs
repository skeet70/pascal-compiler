-- Authored by: Ryan "Murph" Murphy
-- Date: 1/24/2013
--
-- This file contains all the top level methods required for the Scanner to
-- function.

module Scanner
    (
    , getToken
    , skipWhitespace
    , getLexeme
    , getLineNumber
    , getColumnNumber
    ) where

import Data.Char (isDigit, isLetter, isControl, isSpace)

-- | Starts with the input string (assuming at first character of next lexeme),
-- an empty lexeme, and the current line and column numbers.
-- | Returns all the same things, expecting the reciever to extract the final
-- token
getToken :: (String, String, Int, Int) -> (String, String, Int, Int)
getToken (source, lexeme, columnNumber, lineNumber)
    | isSpace nextChar
        = skipWhitespace (nextChar : source, lexeme, columnNumber, lineNumber)
    | nextChar == '('
        = lParenFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | nextChar == ')'
        = rParenFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | nextChar == ';'
        = semicolonFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | nextChar == ':'
        = colonFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | isLetter nextChar
        = letterFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | isDigit nextChar
        = digitFSA (nextChar : source, lexeme, columnNumber, lineNumber)
  where
    nextChar = head source  -- get the next character

-- | skipWhitespace expects to recieve parameters that have already consumed the
-- whitespace or control character, and calls getToken with the
-- modified source and column/line numbers.
skipWhitespace :: (String, String, Int, Int) -> (String, String, Int, Int)
skipWhitespace (source, lexeme, columnNumber, lineNumber)
    | isControl nextChar
        = getToken (source, lexeme, 0, lineNumber + 1)
    | nextChar == ' '
        = getToken (source, lexeme, columnNumber + 1, lineNumber)
  where
    nextChar = head source  -- get the next character

-- | Gets the lexeme currently being passed around and returns it.
getLexeme :: (String, String, Int, Int) -> String
getLexeme (source, lexeme, columnNumber, lineNumber) = lexeme

-- | Gets the line number currently being passed around and returns it.
getLineNumber :: (String, String, Int, Int) -> Int
getLineNumber (source, lexeme, columnNumber, lineNumber) = lineNumber

-- | Gets the column number currently being passed around and returns it.
getColumnNumber :: (String, String, Int, Int) -> Int
getColumnNumber (source, lexeme, columnNumber, lineNumber) = columnNumber
