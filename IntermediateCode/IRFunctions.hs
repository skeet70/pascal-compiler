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

generateProcedureLabel :: ParsingData -> Int -> ParsingData
generateProcedureLabel parsingData procedureLabel
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
                  "L" ++ show procedureLabel ++ ":"
            ]
            , tagAlong = tagAlong parsingData
            , semanticRecord = SemanticRecord {
                    labelNumber = procedureLabel
                  , isFloat = isFloat (semanticRecord parsingData)
                  , idType = idType (semanticRecord parsingData)
            } }

generateProcedureEnd :: ParsingData -> Int -> ParsingData
generateProcedureEnd parsingData procedureLabel
      = ParsingData {
              lookAheadToken = lookAheadToken parsingData
            , hasFailed = hasFailed parsingData
            , line = line parsingData
            , column = column parsingData
            , errorString = errorString parsingData
            , input = input parsingData
            , symbolTables = symbolTables parsingData
            , current_lexeme = current_lexeme parsingData
            , intermediateCode = (intermediateCode parsingData) ++ ["RET"]
            , tagAlong = tagAlong parsingData
            , semanticRecord = semanticRecord parsingData }

--Generates code to pop a value into a given variable
generatePopDestination :: ParsingData -> ScopeData -> ParsingData
generatePopDestination parsingData scopeData = 
      (if isFloat (semanticRecord parsingData) == True && idType (semanticRecord parsingData) /= "integer"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["CASTSF","POP " ++ show (offset scopeData) ++ "(D" ++ (show (level scopeData)) ++ ")"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else if isFloat (semanticRecord parsingData) == True && idType (semanticRecord parsingData) == "integer"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["CASTSI","POP " ++ show (offset scopeData) ++ "(D" ++ (show (level scopeData)) ++ ")"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else if idType (semanticRecord parsingData) == "string"
            then ParsingData {
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
            else ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["CASTSI", "POP " ++ show (offset scopeData) ++ "(D" ++ (show (level scopeData)) ++ ")"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData })

--Generates code to push a literal into the stack
generatePushLiterals :: ParsingData -> ParsingData
generatePushLiterals parsingData = 
      (if unwrapToken (lookAheadToken parsingData) == "MP_STRING_LIT"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ "#" ++ "\"" ++ current_lexeme parsingData ++ "\""]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else if isFloat (semanticRecord parsingData) == True && idType (semanticRecord parsingData) /= "integer"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ "#" ++ current_lexeme parsingData, "CASTSF"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else if isFloat (semanticRecord parsingData) == True && idType (semanticRecord parsingData) == "integer"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ "#" ++ current_lexeme parsingData, "CASTSI"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else  if any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER"] && idType (semanticRecord parsingData) == "float"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ "#" ++ current_lexeme parsingData, "CASTSF"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else  if any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER"] && idType (semanticRecord parsingData) /= "float"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ "#" ++ current_lexeme parsingData, "CASTSI"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else  ParsingData {
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
                        , semanticRecord = semanticRecord parsingData })

--Generates code to push a register onto the stack
generatePushIdentifier :: ParsingData -> ScopeData -> ParsingData
generatePushIdentifier parsingData scopeData = 
      (if varType scopeData == "MP_STRING"
            then ParsingData {
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
            else if isFloat (semanticRecord parsingData) == True && idType (semanticRecord parsingData) /= "integer"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")", "CASTSF"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else if isFloat (semanticRecord parsingData) == True && idType (semanticRecord parsingData) == "integer"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")", "CASTSI"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else if isFloat (semanticRecord parsingData) == False && idType (semanticRecord parsingData) == "float"
            then  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")", "CASTSF"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData }
            else  ParsingData {
                          lookAheadToken = lookAheadToken parsingData
                        , hasFailed = hasFailed parsingData
                        , line = line parsingData
                        , column = column parsingData
                        , errorString = errorString parsingData
                        , input = input parsingData
                        , symbolTables = symbolTables parsingData
                        , current_lexeme = current_lexeme parsingData
                        , intermediateCode = (intermediateCode parsingData) ++ ["PUSH " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")", "CASTSI"]
                        , tagAlong = tagAlong parsingData
                        , semanticRecord = semanticRecord parsingData })


--Increments the stack by one, used for reserving memory for storing variables into registers
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

--Generates code to request data to be read into the program
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

--Generates code to request data to be written to STDOUT
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

generateWriteLineFunction :: ParsingData -> ParsingData
generateWriteLineFunction parsingData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["WRTLN"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

--Generates code to create an "If" function
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
                                    , intermediateCode = (intermediateCode parsingData) ++ ["L" ++ show labelValue ++ ":"]
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
                                    , intermediateCode = (intermediateCode parsingData) ++ ["BR " ++ "L" ++ show labelValue] ++ ["L" ++ show (labelValue-1) ++ ":"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

castToFloatFunction :: ParsingData -> ParsingData
castToFloatFunction parsingData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ ["CASTSF"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

-- Handles any stack modifiers. Checks semantic record for float/int.
generateStackModifier :: ParsingData -> String -> ParsingData
generateStackModifier parsingData operator
      | operator ==  "MP_PLUS" && isFloat (semanticRecord parsingData) == False
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
      | operator ==  "MP_MINUS" && isFloat (semanticRecord parsingData) == False
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
      | operator ==  "MP_TIMES" && isFloat (semanticRecord parsingData) == False
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
      | operator ==  "MP_DIV" && isFloat (semanticRecord parsingData) == False
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
      | operator ==  "MP_MOD" && isFloat (semanticRecord parsingData) == False
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
      | operator ==  "MP_AND" && isFloat (semanticRecord parsingData) == False
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
      | operator ==  "MP_OR" && isFloat (semanticRecord parsingData) == False
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
      | operator ==  "MP_NOT" && isFloat (semanticRecord parsingData) == False
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CASTSF","ADDSF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CASTSF","SUBSF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CASTSF","MULSF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CASTSF","DIVSF"]
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
      | comparison == "MP_EQUALS" && isFloat (semanticRecord parsingData) == False
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
      | comparison == "MP_LTHAN" && isFloat (semanticRecord parsingData) == False
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
      | comparison == "MP_GTHAN" && isFloat (semanticRecord parsingData) == False
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
      | comparison == "MP_LEQUAL" && isFloat (semanticRecord parsingData) == False
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
      | comparison == "MP_GEQUAL" && isFloat (semanticRecord parsingData) == False
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
      | comparison == "MP_NEQUAL" && isFloat (semanticRecord parsingData) == False
            = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPNES"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData }
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPGESF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPLTSF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPGTSF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPLESF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPGESF"]
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
                  , intermediateCode = (intermediateCode parsingData) ++ ["CMPNESF"]
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
                        , idType = idType (semanticRecord parsingData)
                  }}

-- Generates the branch step for a while. Should be done after the comparison
-- and startwhile statements.
generateBranchWhile :: ParsingData -> Int -> ParsingData
generateBranchWhile parsingData whileLabelStart
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
                            "BRFS L" ++ show (whileLabelStart + 2)
                        ]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData
            }

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
                          "BR L" ++ show (whileLabelStart + 1)
                        , "L" ++ show (whileLabelStart + 2) ++ ":"
                  ]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData
            }

-- Takes in parsing data, and outputs code for the start of a while loop. Which
-- means it just sets a label and increments the label counter for both it's
-- label and the end label.
generateStartRepeat :: ParsingData -> ParsingData
generateStartRepeat parsingData
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
                          labelNumber = (labelNumber (semanticRecord parsingData)) + 1
                        , isFloat = isFloat (semanticRecord (parsingData))
                        , idType = idType (semanticRecord parsingData)
                  }}

-- Takes in ParsingData and the label number from the start of the while loop.
-- Outputs code to jump back to the top of the loop, and the label for escaping.
generateEndRepeat :: ParsingData -> Int -> ParsingData
generateEndRepeat parsingData repeatLabelStart
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
                          "BRFS L" ++ show (repeatLabelStart)
                  ]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData
            }

-- Generates a "HLT" statement, and should be run at the end of execution
generateHalt :: ParsingData -> ParsingData
generateHalt parsingData
    = ParsingData {
                    lookAheadToken = lookAheadToken parsingData
                  , hasFailed = hasFailed parsingData
                  , line = line parsingData
                  , column = column parsingData
                  , errorString = errorString parsingData
                  , input = input parsingData
                  , symbolTables = symbolTables parsingData
                  , current_lexeme = current_lexeme parsingData
                  , intermediateCode = (intermediateCode parsingData) ++ ["HLT"]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData
            }

-- Takes in parsing data, and outputs code for the start of a for loop. Which
-- means it just sets a label and increments the label counter for both it's
-- label and the end label.
generateStartFor :: ParsingData -> ParsingData
generateStartFor parsingData
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
                        , idType = idType (semanticRecord parsingData)
                  }}

-- Generates the branch step for a for loop. Should be done after the comparison
-- and startFor statements.
generateBranchFor :: ParsingData -> Int -> ParsingData
generateBranchFor parsingData forLabelStart
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
                            "BRTS L" ++ show (forLabelStart + 2)
                        ]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData
            }

-- Takes in ParsingData and the label number from the start of the for loop.
-- Outputs code to jump back to the top of the loop, and the label for escaping.
generateEndFor :: ParsingData -> Int -> ParsingData
generateEndFor parsingData forLabelStart
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
                          "BR L" ++ show (forLabelStart + 1)
                        , "L" ++ show (forLabelStart + 2) ++ ":"
                  ]
                  , tagAlong = tagAlong parsingData
                  , semanticRecord = semanticRecord parsingData
            }

generateIncrementFunction :: ParsingData -> ScopeData -> ParsingData
generateIncrementFunction  parsingData scopeData = ParsingData {
                                      lookAheadToken = lookAheadToken parsingData
                                    , hasFailed = hasFailed parsingData
                                    , line = line parsingData
                                    , column = column parsingData
                                    , errorString = errorString parsingData
                                    , input = input parsingData
                                    , symbolTables = symbolTables parsingData
                                    , current_lexeme = current_lexeme parsingData
                                    , intermediateCode = (intermediateCode parsingData) ++ 
                                    ["ADD " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")" 
                                    ++ " #1 " ++ (show (offset scopeData)) ++ "(D" ++ (show (level scopeData)) ++ ")"]
                                    , tagAlong = tagAlong parsingData
                                    , semanticRecord = semanticRecord parsingData }

generateDecrementFunction :: ParsingData -> ScopeData -> ParsingData
generateDecrementFunction  parsingData scopeData = ParsingData {
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
