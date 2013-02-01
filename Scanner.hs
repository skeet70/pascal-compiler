-- Authored by: Ryan "Murph" Murphy
-- Date: 1/24/2013
--
-- This file contains all the top level methods required for the Scanner to
-- function.

module Scanner where

import DigitFSA
import LetterFSA
import TokenTable

import Data.Char (isDigit, isLetter, isControl, isSpace)

-- | Starts with the input string (assuming at first character of next lexeme),
-- an empty lexeme, and the current line and column numbers.
-- | Returns all the same things, expecting the reciever to extract the final
-- token
getToken :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
getToken (source, lexeme, columnNumber, lineNumber)
    | null source
        = (source, lexeme, EndOfFile MP_EOF, columnNumber, lineNumber)
    | isSpace nextChar
        = skipWhitespace (source, lexeme, columnNumber, lineNumber)
    | isLetter nextChar || '_' == nextChar
        = identifierFSA (source, lexeme, columnNumber, lineNumber)
    | isDigit nextChar
        = digitFSA (source, lexeme, columnNumber, lineNumber)
    | nextChar == '\'' || nextChar == '"'
        = stringFSA (source, lexeme, columnNumber, lineNumber)
    | nextChar == ':'
        = colonFSA (source, lexeme, columnNumber, lineNumber)
    | nextChar == '>'
        = gthanFSA (source, lexeme, columnNumber, lineNumber)
    | nextChar == '<'
        = lthanFSA (source, lexeme, columnNumber, lineNumber)
    | nextChar == '('
        = (tail source, "(", Symbols MP_LPAREN, columnNumber + 1, lineNumber)
    | nextChar == ')'
        = (tail source, ")", Symbols MP_RPAREN, columnNumber + 1, lineNumber)
    | nextChar == ';'
        = (tail source, ";", Symbols MP_SCOLON, columnNumber + 1, lineNumber)
    | nextChar == '='
        = (tail source, "=", Symbols MP_EQUAL, columnNumber + 1, lineNumber)
    | nextChar == '.'
        = (tail source, ".", Symbols MP_PERIOD, columnNumber + 1, lineNumber)
    | nextChar == ','
        = (tail source, ",", Symbols MP_COMMA, columnNumber + 1, lineNumber)
    | nextChar == '+'
        = (tail source, "+", Symbols MP_PLUS, columnNumber + 1, lineNumber)
    | nextChar == '-'
        = (tail source, "-", Symbols MP_MINUS, columnNumber + 1, lineNumber)
    | nextChar == '*'
        = (tail source, "*", Symbols MP_TIMES, columnNumber + 1, lineNumber)
    | otherwise
        = (tail source, "", ErrorCodes MP_ERROR, columnNumber + 1, lineNumber)
  where
    nextChar = head source  -- get the next character

-- | skipWhitespace expects to recieve parameters that have already consumed
-- the whitespace or control character, and calls getToken with the
-- modified source and column/line numbers.
skipWhitespace :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
skipWhitespace (source, lexeme, columnNumber, lineNumber)
    | isControl nextChar
        = getToken (tail source, lexeme, 0, lineNumber + 1)
    | nextChar == ' '
        = getToken (tail source, lexeme, columnNumber + 1, lineNumber)
  where
    nextChar = head source  -- get the next character

-- | Gets the lexeme currently being passed around and returns it.
getLexeme :: (String, String, Token, Int, Int) -> String
getLexeme (source, lexeme, token, columnNumber, lineNumber) = lexeme

-- | Gets the line number currently being passed around and returns it.
getLineNumber :: (String, String, Token, Int, Int) -> Int
getLineNumber (source, lexeme, token, columnNumber, lineNumber) = lineNumber

-- | Gets the column number currently being passed around and returns it.
getColumnNumber :: (String, String, Token, Int, Int) -> Int
getColumnNumber (source, lexeme, token, columnNumber, lineNumber) = columnNumber
