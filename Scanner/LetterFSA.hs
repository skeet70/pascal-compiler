-- Authored by: James Sonntag.
-- Creation Date: 1/24/2013
-- Most Recent Update: 2/1/2013
-- This file contains the State-Machines that relate to Identifiers, Strings, and Symbol.

module Scanner.LetterFSA where

import Scanner.TokenTable

import Data.Char

-- The FSA that returns anything that can be an Identifier.
-- *Added the ability to: send an error token when there is not a correct leading character after an underscore,
-- send the proper identifier token.
identifierFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
identifierFSA (source, lexeme, colNum, lineNum)
    | null source
        = (source, lexeme, IdentifierOrLiteral MP_IDENTIFIER, colNum, lineNum)
    | isAlpha (head source)
        = identifierFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
    | isAlphaNum (head source) && not(null lexeme)
        = identifierFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
    | (head source) == '_' && (tail source) /= [] && isAlphaNum (head (tail source))
        = identifierFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
    | (head source) == '_' && (tail source) /= [] && not (isAlphaNum (head (tail source)))
        = (tail source, lexeme ++ [head source], IdentifierOrLiteral MP_IDENTIFIER, colNum, lineNum)
    | otherwise
        = (source, lexeme, IdentifierOrLiteral MP_IDENTIFIER, colNum, lineNum)

-- The FSA that returns anything that can be a string.
-- *Added the ability to: send an error token when there is a runaway string,
-- deal with a deliberate new-line character that was put in by the programmer,
-- be functional with two apostrophe's in the string,
-- send the proper tokens.
stringFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
stringFSA (source, lexeme, colNum, lineNum)
    | null source
        = (source, lexeme, ErrorCode MP_RUN_STRING, colNum, lineNum)
    | (head source) == '\'' && (tail source) /= [] && (head (tail source)) == '\'' && null lexeme
        = (tail (tail source), lexeme, IdentifierOrLiteral MP_STRING_LIT, colNum+2, lineNum)
    | (head source) == '\'' && null lexeme
        = stringFSA(tail source, lexeme, colNum+1, lineNum)
    | (head source) == '\'' && (tail source) /= [] && (head (tail source)) == '\''
        = stringFSA(tail (tail source), lexeme ++ [head source], colNum+2, lineNum)
    | (head source) == '\''
        = (tail source, lexeme, IdentifierOrLiteral MP_STRING_LIT, colNum+1, lineNum)
    | (head source) == '\\' && (tail source) /= [] && (head (tail source)) == 'n'  -- same as | take 2 source == "\\n"
        = stringFSA(tail (tail source), lexeme ++ "\n", colNum+2, lineNum)
    | (head source) == '\n'
        = (source, lexeme, ErrorCode MP_RUN_STRING, colNum, lineNum)
    | isAscii (head source)
        = stringFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
    | otherwise
        = (tail source, lexeme, ErrorCode MP_ERROR, colNum, lineNum)

-- The FSA that returns anything that relates to the greater-than character.
gthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
gthanFSA (source, lexeme, colNum, lineNum)
    | (head source) == '>' && (head (tail source)) == '='
        = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbol MP_GEQUAL, colNum+2, lineNum)
    | (head source) == '>'
        = (tail source, lexeme ++ [head source], Symbol MP_GTHAN, colNum+1, lineNum)
    | otherwise
        = (tail source, lexeme, ErrorCode MP_ERROR, colNum, lineNum)

-- The FSA that returns anything that relates to the less-than character.
lthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
lthanFSA (source, lexeme, colNum, lineNum)
    | (head source) == '<' && (head (tail source)) == '='
        = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbol MP_LEQUAL, colNum+2, lineNum) -- tail (tail source) and drop 2 source are the same, up to you James
    | (head source) == '<' && (head (tail source)) == '>'
        = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbol MP_NEQUAL, colNum+2, lineNum)
    | (head source) == '<'
        = (tail source, lexeme ++ [head source], Symbol MP_LTHAN, colNum+1, lineNum)
    | otherwise
        = (tail source, lexeme, ErrorCode MP_ERROR, colNum, lineNum)

-- The FSA that returns anything that relates to the colon character.
colonFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
colonFSA (source, lexeme, colNum, lineNum)
    | (head source) == ':' && (head (tail source)) == '='
        = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbol MP_ASSIGN, colNum+2, lineNum)
    | (head source) == ':'
        = (tail source, lexeme ++ [head source], Symbol MP_COLON, colNum+1, lineNum)
    | otherwise
        = (tail source, lexeme, ErrorCode MP_ERROR, colNum, lineNum)
