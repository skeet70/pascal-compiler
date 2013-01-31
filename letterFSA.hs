-- Authored by: James Sonntag, Ryan "Murph" Murphy, and Tyler Huff-man.
-- Creation Date: 1/24/2013
-- Most Recent Update: 1/30/2013
-- This file contains the State-Machines that relate to Identifiers, Strings, and Symbols.

module LetterFSA where

import Data.Char
import TokenTable

-- The FSA that returns anything that can be an Identifier.
-- *Added the ability to: send an error token when there is not a correct leading character after an underscore,
-- send the proper identifier token.
identifierFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
identifierFSA (source, lexeme, colNum, lineNum)
  | null source                                                   = (source, lexeme, EndOfFile MP_EOF, colNum, lineNum)
  | isAlphaNum (head source)                                      = identifierFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
  | (head source) == '_' && isAlphaNum (head (tail source))       = identifierFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
  | (head source) == '_' && not (isAlphaNum (head (tail source))) = (source, lexeme, ErrorCodes MP_ERROR, colNum, lineNum)
  | otherwise                                                     = (source, lexeme, IdentifiersAndLiterals MP_IDENTIFIER, colNum, lineNum)

-- The FSA that returns anything that can be a string.
-- *Added the ability to: send an error token when there is a runaway string,
-- deal with a deliberate new-line character that was put in by the programmer,
-- be functional with two apostrophe's in the string,
-- send the proper tokens.
stringFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
stringFSA (source, lexeme, colNum, lineNum)
  | null source                                                                                 = (source, lexeme, ErrorCodes MP_RUN_STRING, colNum, lineNum)
  | (head source) == '\'' && (tail source) /= [] && (head (tail source)) == '\'' && null lexeme = (tail (tail source), lexeme, IdentifiersAndLiterals MP_STRING_LIT, colNum+2, lineNum)
  | (head source) == '\'' && null lexeme                                                        = stringFSA(tail source, lexeme, colNum+1, lineNum)
  | (head source) == '\'' && (tail source) /= [] && (head (tail source)) == '\''                = stringFSA(tail (tail source), lexeme ++ [head source], colNum+2, lineNum)
  | (head source) == '\''                                                                       = (tail source, lexeme, IdentifiersAndLiterals MP_STRING_LIT, colNum+1, lineNum)
  | (head source) == '\\' && (tail source) /= [] && (head (tail source)) == 'n'                 = stringFSA(tail (tail source), lexeme ++ "\n", colNum+2, lineNum)
  | (head source) == '\n'                                                                       = (source, lexeme, ErrorCodes MP_RUN_STRING, colNum, lineNum)
  | isAscii (head source)                                                                       = stringFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
  | otherwise                                                                                   = (tail source, lexeme, ErrorCodes MP_ERROR, colNum, lineNum)

-- The FSA that returns anything that relates to the greater-than character.
-- *Added the ability to: send an error token when there an unknown character,
-- send the proper tokens.
gthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
gthanFSA (source, lexeme, colNum, lineNum) 
  | (head source) == '>' && (head (tail source)) == '=' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbols MP_GEQUAL, colNum+2, lineNum)
  | (head source) == '>'                                = (tail source, lexeme ++ [head source], Symbols MP_GTHAN, colNum+1, lineNum)
  | otherwise                                           = (tail source, lexeme, ErrorCodes MP_ERROR, colNum, lineNum)

-- The FSA that returns anything that relates to the less-than character.
-- *Added the ability to: send an error token when there an unknown character,
-- send the proper tokens.
lthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
lthanFSA (source, lexeme, colNum, lineNum)
  | (head source) == '<' && (head (tail source)) == '=' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbols MP_LEQUAL, colNum+2, lineNum)
  | (head source) == '<' && (head (tail source)) == '>' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbols MP_NEQUAL, colNum+2, lineNum)
  | (head source) == '<'                                = (tail source, lexeme ++ [head source], Symbols MP_LTHAN, colNum+1, lineNum)
  | otherwise                                           = (tail source, lexeme, ErrorCodes MP_ERROR, colNum, lineNum)

-- The FSA that returns anything that relates to the colon character.
-- *Added the ability to: send an error token when there an unknown character,
-- send the proper tokens.
colonFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
colonFSA (source, lexeme, colNum, lineNum)
  | (head source) == ':' && (head (tail source)) == '=' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], Symbols MP_ASSIGN, colNum+2, lineNum)
  | (head source) == ':'                                = (tail source, lexeme ++ [head source], Symbols MP_COLON, colNum+1, lineNum)
  | otherwise                                           = (tail source, lexeme, ErrorCodes MP_ERROR, colNum, lineNum)