--Intermediate Code Generation Functions
--
--Authored by: Tyler J. Huffman
--
--Edited: Feb. 19, 2013

module IntermediateCode.IRFunctions where

import Debug.Trace
import Parser.ParsingData

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
                                    , tagAlong = tagAlong parsingData }
