-- Authored by: James Sonntag, Ryan "Murph" Murphy, and Tyler Huffman.
-- Creation Date: 1/24/2013
-- Most Recent Update: 1/30/2013
-- This file contains the State-Machines that relate to Identifiers, Strings, and Symbols.

module Scanner.LetterFSA where

import Data.Char
import Scanner.TokenTable
import Data.Maybe

-- The FSA that returns anything that can be an Identifier.
-- *Added the ability to: send an error token when there is not a correct leading character after an underscore,
-- send the proper identifier token.
identifierFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
identifierFSA (source, lexeme, colNum, lineNum)
  | isAlphaNum (fromJust stringHead)                                   = identifierFSA(tail source, lexeme ++ (charToString (fromJust stringHead)), colNum+1, lineNum)
  | stringHead == Just '_' && isAlphaNum (fromJust stringNext)         = identifierFSA(tail source, lexeme ++ (charToString (fromJust stringHead)), colNum+1, lineNum)
  | null source                                                         = (source, lexeme, IdentifiersAndLiterals MP_IDENTIFIER, colNum, lineNum)
  | stringHead == Just '_' && not (isAlphaNum (fromJust stringNext))   = (source, lexeme, IdentifiersAndLiterals MP_IDENTIFIER, colNum, lineNum)
  | otherwise                                                           = (source, lexeme, IdentifiersAndLiterals MP_IDENTIFIER, colNum, lineNum)
  where stringHead = if source == [] then Nothing else Just (head source)
        stringNext
              | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
              | otherwise = Nothing
-- The FSA that returns anything that can be a string.
-- *Added the ability to: send an error token when there is a runaway string,
-- deal with a deliberate new-line character that was put in by the programmer,
-- be functional with two apostrophe's in the string,
-- send the proper tokens.
stringFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
stringFSA (source, lexeme, colNum, lineNum)
  | (stringHead == Just '\'' || stringHead == Just '"') && null lexeme              = stringFSA(tail source, lexeme, colNum+1, lineNum)
  | stringHead == Just '\'' && stringNext == Just '\''                = stringFSA(tail (tail source), lexeme ++ (charToString (fromJust stringHead)), colNum+2, lineNum)
  | stringHead == Just '\\' && stringNext == Just 'n'                 = stringFSA(tail (tail source), lexeme ++ "\n", colNum+2, lineNum)
  | isAscii (fromJust stringHead)                                     = stringFSA(tail source, lexeme ++ (charToString (fromJust stringHead)), colNum+1, lineNum)
  | stringHead == Just '\'' && stringNext == Just '\'' && null lexeme     = (tail (tail source), lexeme, IdentifiersAndLiterals MP_STRING_LIT, colNum+2, lineNum)
  | (stringHead == Just '\'' || stringHead == Just '"')                                 = (tail source, lexeme, IdentifiersAndLiterals MP_STRING_LIT, colNum+1, lineNum)
  | stringHead == Just '\n'                                               = (source, lexeme, IdentifiersAndLiterals MP_STRING_LIT, colNum, lineNum) 
  | null source                                                           = (source, lexeme, IdentifiersAndLiterals MP_STRING_LIT, colNum, lineNum)
  | otherwise                                                             = (tail source, lexeme, IdentifiersAndLiterals MP_STRING_LIT, colNum, lineNum)
  where stringHead = if source == [] then Nothing else Just (head source)
        stringNext
              | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
              | otherwise = Nothing

-- The FSA that returns anything that relates to the greater-than character.
-- *Added the ability to: send an error token when there an unknown character,
-- send the proper tokens.
gthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
gthanFSA (source, lexeme, colNum, lineNum) 
  | stringHead == Just '>' && stringNext == Just '='        = (tail (tail source), lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbols MP_GEQUAL, colNum+2, lineNum)
  | stringHead == Just '>'                                  = (tail source, lexeme ++ (charToString (fromJust stringHead)), Symbols MP_GTHAN, colNum+1, lineNum)
  where stringHead = if source == [] then Nothing else Just (head source)
        stringNext
              | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
              | otherwise = Nothing
-- The FSA that returns anything that relates to the less-than character.
-- *Added the ability to: send an error token when there an unknown character,
-- send the proper tokens.
lthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
lthanFSA (source, lexeme, colNum, lineNum)
  | stringHead == Just '<' && stringNext == Just '='        = (tail (tail source), lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbols MP_LEQUAL, colNum+2, lineNum)
  | stringHead == Just '<' && stringNext == Just '>'        = (tail (tail source), lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbols MP_NEQUAL, colNum+2, lineNum)
  | stringHead == Just '<'                                  = (tail source, lexeme ++ (charToString (fromJust stringHead)), Symbols MP_LTHAN, colNum+1, lineNum)
  where stringHead = if source == [] then Nothing else Just (head source)
        stringNext
              | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
              | otherwise = Nothing
-- The FSA that returns anything that relates to the colon character.
-- *Added the ability to: send an error token when there an unknown character,
-- send the proper tokens.
colonFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
colonFSA (source, lexeme, colNum, lineNum)
  | stringHead == Just ':' && stringNext == Just '=' = (tail (tail source), lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbols MP_ASSIGN, colNum+2, lineNum)
  | stringHead == Just ':'                                = (tail source, lexeme ++ (charToString (fromJust stringHead)), Symbols MP_COLON, colNum+1, lineNum)
  where stringHead = if source == [] then Nothing else Just (head source)
        stringNext
              | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
              | otherwise = Nothing

--Helper function that turns a Char into a String
--
--Parameters: Char
--Returns: String
charToString :: Char -> String
charToString c = [c]
