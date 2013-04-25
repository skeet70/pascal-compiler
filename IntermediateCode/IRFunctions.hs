--Intermediate Code Generation Functions
--
--Authored by: James Sonntag, Tyler J. Huffman, Murph "Ryan" Murphy
--
--Edited: Feb. 19, 2013

module IntermediateCode.IRFunctions where

import Debug.Trace
import Parser.ParsingData
import Parser.Helper
import Scanner.TokenTable
import IntermediateCode.IRHelpers

generatePopDestination :: ParsingData -> ScopeData -> ParsingData
generatePopDestination parsingData scopeData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["POP " ++ show (offset scopeData) ++ "(D" ++ (show (level scopeData)) ++ ")"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

generatePushLiterals :: ParsingData -> ParsingData
generatePushLiterals parsingData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ "#" ++ current_lexeme parsingData]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

generatePushIdentifier :: ParsingData -> ScopeData -> ParsingData
generatePushIdentifier parsingData scopeData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

generateStackIncrement :: ParsingData -> ParsingData
generateStackIncrement parsingData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["ADD SP #1 SP"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

generateReadFunction :: ParsingData -> ScopeData -> ParsingData
generateReadFunction parsingData scopeData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["RD " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

generateWriteFunction :: ParsingData -> ParsingData
generateWriteFunction parsingData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["WRTS"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

generateIfFunction :: ParsingData -> Int -> ParsingData
generateIfFunction parsingData labelValue = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["BRFS " ++ "L" ++ show labelValue]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

insertElseLabel :: ParsingData -> Int -> ParsingData
insertElseLabel parsingData labelValue = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["L" ++ show labelValue]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

insertIfLabelFunction :: ParsingData -> Int -> ParsingData
insertIfLabelFunction parsingData labelValue = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["BR " ++ "L" ++ show labelValue] ++ ["L" ++ show (labelValue-1)]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

--Need to determine if integer or float before doing it.
generateStackModifierInteger :: ParsingData -> String -> ParsingData
generateStackModifierInteger parsingData operator
      | operator ==  "MP_PLUS"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ADDS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_MINUS"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["SUBS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_TIMES"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["MULS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_DIV"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["DIVS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_MOD"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["MODS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_AND"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ANDS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_OR"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ORS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_NOT"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["NOTS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | otherwise
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ERROR"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }

-- Generates IR code for a stack modifier, for fixed/floats.
generateStackModifierFloat :: ParsingData -> String -> ParsingData
generateStackModifierFloat parsingData operator
      | operator ==  "MP_PLUS"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ADDSF"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_MINUS"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["SUBSF"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_TIMES"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["MULSF"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_DIV"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["DIVSF"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_AND"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ANDS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_OR"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ORS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | operator ==  "MP_NOT"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["NOTS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | otherwise
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["ERROR"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }

-- Called with ParsingData and the token of the operation you want to branch on.
-- Generates the code for the comparison and branch.
generateComparison :: ParsingData -> String -> ParsingData
generateComparison parsingData comparison
      | comparison == "MP_EQUALS"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPGES"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | comparison == "MP_LTHAN"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPLTS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | comparison == "MP_GTHAN"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPGTS"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | comparison == "MP_LEQUAL"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPLES"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | comparison == "MP_GEQUAL"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPGES"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
      | comparison == "MP_NEQUAL"
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPGES"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }

-- Takes in parsing data, and outputs code for the start of a while loop. Which
-- means it just sets a label and increments the label counter for both it's
-- label and the end label.
generateStartWhile :: ParsingData -> ParsingData
generateStartWhile parsingData
      = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ [
                          "L" ++ show (labelNumber (semanticRecord parsingData) + 1) ++ ":"
                        ]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = SemanticRecord {
                          labelNumber = (labelNumber (semanticRecord parsingData)) + 2
                        , isFloat = isFloat (semanticRecord (parsingData))
                  }}

-- Takes in ParsingData and the label number from the start of the while loop.
-- Outputs code to jump back to the top of the loop, and the label for escaping.
generateEndWhile :: ParsingData -> Int -> ParsingData
generateEndWhile parsingData whileLabelStart
      = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ [
                          "BR " ++ show (whileLabelStart + 1)
                        , "L" ++ show (whileLabelStart + 2) ++ ":"
                  ]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = SemanticRecord {
                          labelNumber = (labelNumber (semanticRecord parsingData))
                        , isFloat = isFloat (semanticRecord (parsingData))
                  }}


