module Parser.Helper where

import Parser.ParsingData
import Scanner.TokenTable
import Scanner.ScannerData
import Data.List
import IntermediateCode.IRHelpers

import Debug.Trace


-- This is called any time a rule cannot successfully match the current token.
-- It returns the same ParsingData object with the hasFailed flag tripped.
syntaxError :: String -> ParsingData -> ParsingData
syntaxError errorList parsingData = ParsingData {   lookAheadToken=(lookAheadToken parsingData)
                                        , hasFailed=True
                                        , line=(line parsingData)
                                        , column=(column parsingData)
                                        , input=(input parsingData)
                                        , symbolTables=(symbolTables parsingData)
                                        , errorString="Expected " ++ errorList ++ " but found " ++ unwrapToken (lookAheadToken parsingData)
                                        , intermediateCode = intermediateCode parsingData
                                        , semanticRecord = semanticRecord parsingData
                                    }

-- Generic called whenever a terminal is encountered. Gets the next token from
-- the input list, as well as the corresponding line_scan and column_scan.
match :: ParsingData -> ParsingData
match parsingData = ParsingData {     lookAheadToken=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a )
r_paren_match :: ParsingData -> ParsingData
r_paren_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_RPAREN"
        = ParsingData {   lookAheadToken=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then EndOfFile MP_EOF
                            else (token (head(tail(input parsingData))))
                        , hasFailed=False
                        , line=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (line_scan (head(tail(input parsingData))))
                        , column=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (column_scan (head(tail(input parsingData))))
                        , input=if
                            length (input parsingData) == 0
                            then []
                            else (tail (input parsingData))
                        , symbolTables=(symbolTables parsingData)
                        , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                        , intermediateCode = intermediateCode parsingData
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData
                    }
    | otherwise
        = syntaxError "MP_RPAREN" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a (
l_paren_match :: ParsingData -> ParsingData
l_paren_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = ParsingData {   lookAheadToken=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then EndOfFile MP_EOF
                            else (token (head(tail(input parsingData))))
                        , hasFailed=False
                        , line=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (line_scan (head(tail(input parsingData))))
                        , column=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (column_scan (head(tail(input parsingData))))
                        , input=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then []
                            else (tail (input parsingData))
                        , symbolTables=(symbolTables parsingData)
                        , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                        , intermediateCode = intermediateCode parsingData
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData
                    }
    | otherwise
        = syntaxError "MP_LPAREN" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a :=
assignment_match :: ParsingData -> ParsingData
assignment_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_ASSIGN"
        = ParsingData {   lookAheadToken=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then EndOfFile MP_EOF
                            else (token (head(tail(input parsingData))))
                        , hasFailed=False
                        , line=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (line_scan (head(tail(input parsingData))))
                        , column=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (column_scan (head(tail(input parsingData))))
                        , input=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then []
                            else (tail (input parsingData))
                        , symbolTables=(symbolTables parsingData)
                        , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                        , intermediateCode = intermediateCode parsingData
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData
                    }
    | otherwise
        = syntaxError "MP_ASSIGN" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a then
then_match :: ParsingData -> ParsingData
then_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_THEN"
        = ParsingData {   lookAheadToken=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then EndOfFile MP_EOF
                            else (token (head(tail(input parsingData))))
                        , hasFailed=False
                        , line=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (line_scan (head(tail(input parsingData))))
                        , column=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then 0
                            else (column_scan (head(tail(input parsingData))))
                        , input=if
                            length (input parsingData) == 0 ||
                            length (input parsingData) == 1
                            then []
                            else (tail (input parsingData))
                        , symbolTables=(symbolTables parsingData)
                        , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                        , intermediateCode = intermediateCode parsingData
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData
                    }
    | otherwise
        = syntaxError "MP_THEN" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be an until
until_match :: ParsingData -> ParsingData
until_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_UNTIL"
        = ParsingData {     lookAheadToken=if 
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_UNTIL" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a do
do_match :: ParsingData -> ParsingData
do_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_DO"
        = ParsingData {     lookAheadToken=if 
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_DO" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a ;
semic_match :: ParsingData -> ParsingData
semic_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_SCOLON"
        = ParsingData {     lookAheadToken=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_SCOLON" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a .
period_match :: ParsingData -> ParsingData
period_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PERIOD"
        = ParsingData {     lookAheadToken=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_PERIOD" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a :
colon_match :: ParsingData -> ParsingData
colon_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COLON"
        = ParsingData {     lookAheadToken=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_COLON" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a "end"
end_match :: ParsingData -> ParsingData
end_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_END"
        = ParsingData {     lookAheadToken=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_END" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be an identifier
ident_match :: ParsingData -> ParsingData
ident_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = ParsingData {     lookAheadToken=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , current_lexeme= lexeme_scan(head(tail (input parsingData)))
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be an end-of-file
eof_match :: ParsingData -> ParsingData
eof_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_EOF"
        = ParsingData {     lookAheadToken=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then EndOfFile MP_EOF
                                        else (token (head(tail(input parsingData))))
                                    , hasFailed=False
                                    , line=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (line_scan (head(tail(input parsingData))))
                                    , column=if
                                        length (input parsingData) == 0 ||
                                        length (input parsingData) == 1
                                        then 0
                                        else (column_scan (head(tail(input parsingData))))
                                    , input=if
                                        length (input parsingData) == 0
                                        then []
                                        else (tail (input parsingData))
                                    , symbolTables=(symbolTables parsingData)
                                    , intermediateCode = intermediateCode parsingData
                                    , errorString=";Parse complete, stop poking me!"
                                    , semanticRecord = semanticRecord parsingData
                                }
    | otherwise
        = syntaxError "MP_EOF" parsingData

typeInsert :: ParsingData -> [String] -> String -> ParsingData
typeInsert parsingData listData givenType --BAD SHIT BRO! 
    | checker /= givenType
        = typeInsert otherNewParseData  newListData givenType
    | otherwise
        = insertData newParsingData scopeData
      where
        checker = 
            (if (length listData >= 3) 
            then listData!!2
            else last listData)

        scopeData = ScopeData {   name = listData!!1
                                , kind = listData!!0
                                , varType = givenType
                                , offset = length (values (last (symbolTables parsingData)))
                                , level = 0}
        newListData = delete (listData!!1) listData
        newParsingData = ParsingData {    lookAheadToken = lookAheadToken parsingData
                                        , hasFailed = hasFailed parsingData
                                        , line = line parsingData
                                        , column = column parsingData
                                        , errorString = errorString parsingData
                                        , input = input parsingData
                                        , symbolTables = symbolTables parsingData
                                        , current_lexeme = current_lexeme parsingData
                                        , intermediateCode = intermediateCode parsingData
                                        , tagAlong = []
                                        , semanticRecord = semanticRecord parsingData }
        otherNewParseData = insertData parsingData scopeData

procedureAndFunctionInsert :: ParsingData -> [String] -> String -> ParsingData
procedureAndFunctionInsert parsingData listData givenType
    | checker /= givenType
        = procedureAndFunctionInsert otherNewParseData  newListData givenType
    | otherwise
        = insertData newParsingData scopeData
      where
        checker = listData!!1
        scopeData = ScopeData {   name = listData!!0
                                , kind = "Parameter"
                                , varType = givenType
                                , offset = length (values (last (symbolTables parsingData)))
                                , level = 0}
        newListData = delete (listData!!0) listData
        newParsingData = ParsingData {    lookAheadToken = lookAheadToken parsingData
                                        , hasFailed = hasFailed parsingData
                                        , line = line parsingData
                                        , column = column parsingData
                                        , errorString = errorString parsingData
                                        , input = input parsingData
                                        , symbolTables = symbolTables parsingData
                                        , current_lexeme = current_lexeme parsingData
                                        , intermediateCode = intermediateCode parsingData
                                        , tagAlong = [] 
                                        , semanticRecord = semanticRecord parsingData}
        otherNewParseData = insertData parsingData scopeData

--showTables :: [SymbolTable] -> [SymbolTable]
--showTables symbolTables
--    | length symbolTables /= 0
--        = trace(outputString) showTables symbolTables
--      where
--        newVals = values (head symbolTables)
--        outputString = [name newScope] ++ [kind newScope] ++ [varType newScope] ++ [show $ offset newScope]
--            where
--                newScope = head newVals

getNextLabelForIf :: ParsingData -> ParsingData
getNextLabelForIf parsingData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = intermediateCode parsingData
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = newSemRecord }
    where
        newSemRecord = SemanticRecord { labelNumber = (labelNumber (semanticRecord parsingData)) + 2
                                      , isFloat = isFloat (semanticRecord parsingData)
                                      , idType = idType (semanticRecord parsingData)
                                      }