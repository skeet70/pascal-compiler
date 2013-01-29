-- Authored by: James Sonntag
-- Date: 1/24/2013
--
-- This file contains all the methods that deal with letters and characters

module LetterFSA where

import Data.Char

-- The FSA that returns anything that can be an Identifier
identifierFSA (source, lexeme, colNum, lineNum)
  | null source              = (source, lexeme, colNum, lineNum)
  | isAlphaNum (head source) = identifierFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
  | (head source) == '_'     = identifierFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
  | otherwise                = (source, lexeme, colNum, lineNum)

-- The FSA that returns anything that can be a string
stringFSA (source, lexeme, colNum, lineNum)
  | null source                                                                  = (source, lexeme, colNum, lineNum)
  | (head source) == '\'' && (tail source) /= [] && (head (tail source)) == '\'' = stringFSA(tail (tail source), lexeme ++ [head source] ++ [head (tail source)], colNum+2, lineNum)
  | (head source) == '\'' && null lexeme                                         = stringFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
  | (head source) == '\'' && (head lexeme) == '\''                               = (tail source, lexeme ++ [head source], colNum+1, lineNum)
  | (head source) == '\n'                                                        = (source, lexeme, colNum, lineNum)
  | isAscii (head source)                                                        = stringFSA(tail source, lexeme ++ [head source], colNum+1, lineNum)
  | otherwise                                                                    = (source, lexeme, colNum, lineNum)

-- The FSA that returns anything that relates to the greater-than character
gthanFSA (source, lexeme, colNum, lineNum) 
  | (head source) == '>' && (head (tail source)) == '=' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], colNum+2, lineNum)
  | (head source) == '>'                                = (tail source, lexeme ++ [head source], colNum+1, lineNum)
  | otherwise                                           = (source, lexeme, colNum, lineNum)

-- The FSA that returns anything that relates to the less-than character
lthanFSA (source, lexeme, colNum, lineNum)
  | (head source) == '<' && (head (tail source)) == '=' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], colNum+2, lineNum)
  | (head source) == '<' && (head (tail source)) == '>' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], colNum+2, lineNum)
  | (head source) == '<'                                = (tail source, lexeme ++ [head source], colNum+1, lineNum)
  | otherwise                                           = (source, lexeme, colNum, lineNum)

-- The FSA that returns anything that relates to the colon character
colonFSA (source, lexeme, colNum, lineNum)
  | (head source) == ':' && (head (tail source)) == '=' = (tail (tail source), lexeme ++ [head source] ++ [head (tail source)], colNum+2, lineNum)
  | (head source) == ':'                                = (tail source, lexeme ++ [head source], colNum+1, lineNum)
  | otherwise                                           = (source, lexeme, colNum, lineNum)