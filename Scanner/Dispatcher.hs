-- Authored by: Ryan "Murph" Murphy, Tyler J. Huffman
-- Date: 1/24/2013
--
-- This file contains all the top level methods required for the scanner to
-- function.

module Scanner.Dispatcher where

import Scanner.DigitFSA
import Scanner.LetterFSA
import Scanner.TokenTable

import Data.Maybe

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
    | nextChar == '{'
        = commentFSA (source, lexeme, columnNumber, lineNumber)
    | nextChar == '(' && take 2 source == "(*"
        = commentFSA (source, lexeme, columnNumber, lineNumber)
    | nextChar == '(' && take 2 source /= "(*"
        = (tail source, "(", Symbol MP_LPAREN, columnNumber + 1, lineNumber)
    | nextChar == ')'
        = (tail source, ")", Symbol MP_RPAREN, columnNumber + 1, lineNumber)
    | nextChar == ';'
        = (tail source, ";", Symbol MP_SCOLON, columnNumber + 1, lineNumber)
    | nextChar == '='
        = (tail source, "=", Symbol MP_EQUAL, columnNumber + 1, lineNumber)
    | nextChar == '.'
        = (tail source, ".", Symbol MP_PERIOD, columnNumber + 1, lineNumber)
    | nextChar == ','
        = (tail source, ",", Symbol MP_COMMA, columnNumber + 1, lineNumber)
    | nextChar == '+'
        = (tail source, "+", Symbol MP_PLUS, columnNumber + 1, lineNumber)
    | nextChar == '-'
        = (tail source, "-", Symbol MP_MINUS, columnNumber + 1, lineNumber)
    | nextChar == '*'
        = (tail source, "*", Symbol MP_TIMES, columnNumber + 1, lineNumber)
    | otherwise
        = (tail source, "", ErrorCode MP_ERROR, columnNumber + 1, lineNumber)
  where
    nextChar = head source  -- get the next character

-- | skipWhitespace expects to recieve parameters that have already consumed
-- the whitespace or control character, and calls getToken with the
-- modified source and column/line numbers.
skipWhitespace :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
skipWhitespace (source, lexeme, columnNumber, lineNumber)
    | isControl nextChar
        = getToken (tail source, lexeme, 1, lineNumber + 1)
    | nextChar == ' '
        = getToken (tail source, lexeme, columnNumber + 1, lineNumber)
  where
    nextChar = head source  -- get the next character

-- | Top-level state machine to remove comments from a source string
-- | Returns the ErrorCode MP_RUN_STRING token if no FSA is valid
commentFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
commentFSA (source, lexeme, columnNumber, lineNumber)
    | stringHead == Just '{'
        = bracketFSA (tail source, lexeme, columnNumber + 1, lineNumber)
    | stringHead == Just '(' && stringNext == Just '*'
        = parenFSA (drop 2 source, lexeme, columnNumber + 1, lineNumber)
    | otherwise
        = (tail source, lexeme, ErrorCode MP_ERROR, columnNumber + 1, lineNumber)
  where stringHead = if null source then Nothing else Just (head source)
        stringNext
            | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
            | otherwise = Nothing

-- | Low-level state machine to run when a { comment is found
-- | Returns the IdentifierOrLiteral MP_STRING_LIT token
bracketFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
bracketFSA (source, lexeme, columnNumber, lineNumber)
    | stringHead == Just '}'
        = getToken (tail source, lexeme , columnNumber + 1, lineNumber)
    | stringHead == Nothing
        = (source, lexeme, ErrorCode MP_RUN_COMMENT, columnNumber, lineNumber)
    | isControl (fromJust stringHead)
        = bracketFSA(tail source, lexeme , 1, lineNumber + 1)
    | otherwise
        = bracketFSA(tail source, lexeme, columnNumber + 1, lineNumber)
  where stringHead = if source == [] then Nothing else Just (head source)

-- | Low-level state machine to run when a (* comment is found
-- | Returns the IdentifierOrLiteral MP_STRING_LIT token
parenFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
parenFSA (source, lexeme, columnNumber, lineNumber)
    | stringHead == Just '*' && stringNext == Just ')'
        = getToken (drop 2 source, lexeme, columnNumber + 1, lineNumber)
    | stringHead == Nothing
        = (source, lexeme, ErrorCode MP_RUN_COMMENT, columnNumber, lineNumber)
    | isControl (fromJust stringHead)
        = parenFSA(tail source, lexeme , 1, lineNumber + 1)
    | otherwise
        = parenFSA (tail source, lexeme, columnNumber + 1, lineNumber)
  where stringHead = if source == [] then Nothing else Just (head source)
        stringNext
            | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
            | otherwise = Nothing

-- | Gets the lexeme currently being passed around and returns it.
getLexeme :: (String, String, Token, Int, Int) -> String
getLexeme (source, lexeme, token, columnNumber, lineNumber) = lexeme

-- | Gets the line number currently being passed around and returns it.
getLineNumber :: (String, String, Token, Int, Int) -> Int
getLineNumber (source, lexeme, token, columnNumber, lineNumber) = lineNumber

-- | Gets the column number currently being passed around and returns it.
getColumnNumber :: (String, String, Token, Int, Int) -> Int
getColumnNumber (source, lexeme, token, columnNumber, lineNumber) = columnNumber
