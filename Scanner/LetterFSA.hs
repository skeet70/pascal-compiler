-- Authored by: James Sonntag, Ryan "Murph" Murphy, and Tyler Huffman.
-- Creation Date: 1/24/2013
-- Most Recent Update: 1/30/2013
-- This file contains the State-Machines that relate to Identifiers, Strings, and Symbol.

module Scanner.LetterFSA where

import Scanner.TokenTable

import Data.Char
import Data.Maybe

-- The FSA that returns anything that can be an Identifier.
-- *Added the ability to: send an error token when there is not a correct leading character after an underscore,
-- send the proper identifier token.
identifierFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
identifierFSA (source, lexeme, colNum, lineNum)
    | isAlphaNum (fromJust stringHead)
        = identifierFSA(tail source, lexeme ++ (charToString (fromJust stringHead)), colNum+1, lineNum)
    | stringHead == Just '_' && isAlphaNum (fromJust stringNext)
        = identifierFSA(tail source, lexeme ++ (charToString (fromJust stringHead)), colNum+1, lineNum)
    | null source
        = (source, lexeme, IdentifierOrLiteral MP_IDENTIFIER, colNum, lineNum)
    | stringHead == Just '_' && not (isAlphaNum (fromJust stringNext))
        = (source, lexeme, IdentifierOrLiteral MP_IDENTIFIER, colNum, lineNum)
    | otherwise
        = (source, lexeme, IdentifierOrLiteral MP_IDENTIFIER, colNum, lineNum)
  where
    stringHead = if null source then Nothing else Just (head source)
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
    | (stringHead == Just '\'' || stringHead == Just '"') && null lexeme
        = stringFSA(tail source, lexeme, colNum+1, lineNum)
    | stringHead == Just '\'' && stringNext == Just '\''
        = stringFSA(drop 2 source, lexeme ++ (charToString (fromJust stringHead)), colNum+2, lineNum)
    | stringHead == Just '\'' && stringNext == Just '\'' && null lexeme
        = (drop 2 source, lexeme, IdentifierOrLiteral MP_STRING_LIT, colNum+2, lineNum)
    | (stringHead == Just '\'' || stringHead == Just '"')
        = (tail source, lexeme, IdentifierOrLiteral MP_STRING_LIT, colNum+1, lineNum)
    | stringHead == Just '\n'
        = (source, lexeme, ErrorCode MP_RUN_STRING, colNum, lineNum)
    | stringHead == Nothing
        = (source, lexeme, ErrorCode MP_RUN_STRING, colNum, lineNum)
    | otherwise
        = stringFSA(tail source, lexeme ++ (charToString (fromJust stringHead)), colNum+1, lineNum)
  where
    stringHead = if null source then Nothing else Just (head source)
    stringNext
        | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
        | otherwise = Nothing

-- The FSA that returns anything that relates to the greater-than character.
gthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
gthanFSA (source, lexeme, colNum, lineNum)
    | stringHead == Just '>' && stringNext == Just '='
        = (drop 2 source, lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbol MP_GEQUAL, colNum+2, lineNum)
    | stringHead == Just '>'
        = (tail source, lexeme ++ (charToString (fromJust stringHead)), Symbol MP_GTHAN, colNum+1, lineNum)
    | otherwise
        = (source, lexeme, ErrorCode MP_ERROR, colNum, lineNum) --Should never hit this line
  where
    stringHead = if null source then Nothing else Just (head source)
    stringNext
        | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
        | otherwise = Nothing

-- The FSA that returns anything that relates to the less-than character.
lthanFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
lthanFSA (source, lexeme, colNum, lineNum)
    | stringHead == Just '<' && stringNext == Just '='
        = (drop 2 source, lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbol MP_LEQUAL, colNum+2, lineNum)
    | stringHead == Just '<' && stringNext == Just '>'
        = (drop 2 source, lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbol MP_NEQUAL, colNum+2, lineNum)
    | stringHead == Just '<'
        = (tail source, lexeme ++ (charToString (fromJust stringHead)), Symbol MP_LTHAN, colNum+1, lineNum)
    | otherwise
        = (source, lexeme, ErrorCode MP_ERROR, colNum, lineNum) --Should never hit this line
  where
    stringHead = if null source then Nothing else Just (head source)
    stringNext
        | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
        | otherwise = Nothing

-- The FSA that returns anything that relates to the colon character.
colonFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
colonFSA (source, lexeme, colNum, lineNum)
    | stringHead == Just ':' && stringNext == Just '='
        = (drop 2 source, lexeme ++ (charToString (fromJust stringHead)) ++ (charToString (fromJust stringNext)), Symbol MP_ASSIGN, colNum+2, lineNum)
    | stringHead == Just ':'
        = (tail source, lexeme ++ (charToString (fromJust stringHead)), Symbol MP_COLON, colNum+1, lineNum)
    | otherwise
        = (source, lexeme, ErrorCode MP_ERROR, colNum, lineNum) --Should never hit this line
  where
    stringHead = if null source then Nothing else Just (head source)
    stringNext
        | stringHead /= Nothing && tail source /= [] = Just (source !! 1)
        | otherwise = Nothing

--Helper function that turns a Char into a String
--
--Parameters: Char
--Returns: String
charToString :: Char -> String
charToString c = [c]
