module Parser.ParsingData where

import Scanner.TokenTable
import Scanner.ScannerData

import Debug.Trace

data ParsingData = ParsingData {  lookAheadToken :: Token
                                , hasFailed :: Bool
                                , line :: Int
                                , column :: Int
                                , errorString :: String
                                , input :: [ScannerData]
                                , symbolTables :: [SymbolTable]
                                , current_lexeme :: String
                                , tagAlong :: [String]
                                } deriving (Show)

data SymbolTable = SymbolTable {  values :: [ScopeData] } deriving (Show)

data ScopeData = ScopeData {  name :: String
                            , kind :: String
                            , varType :: String
                            , attribute :: String
                            , offset :: Int
                            } deriving (Show)

create :: ParsingData -> ParsingData
create parsingData
    = ParsingData { lookAheadToken=(lookAheadToken parsingData)
                  , hasFailed=(hasFailed parsingData)
                  , line=(line parsingData)
                  , column=(column parsingData)
                  , input=(input parsingData)
                  , symbolTables=(symbolTables parsingData ++ [SymbolTable { values=[] }])
                  , current_lexeme=lexeme_scan(head (input parsingData))
                  , tagAlong = tagAlong parsingData
                }

destroy :: ParsingData -> ParsingData
destroy parsingData
    = ParsingData { lookAheadToken=(lookAheadToken parsingData)
                  , hasFailed=(hasFailed parsingData)
                  , line=(line parsingData)
                  , column=(column parsingData)
                  , input=(input parsingData)
                  , symbolTables=(init (symbolTables parsingData))
                  , current_lexeme=lexeme_scan(head (input parsingData))
                  , tagAlong = tagAlong parsingData
                }

insertData :: ParsingData -> ScopeData -> ParsingData
insertData parsingData scopeData
    =  ParsingData { lookAheadToken=(lookAheadToken parsingData)
                  , hasFailed=(hasFailed parsingData)
                  , line=(line parsingData)
                  , column=(column parsingData)
                  , input=(input parsingData)
                  , symbolTables=(newTables ++ [SymbolTable { values=newVals }])
                  , current_lexeme=lexeme_scan(head (input parsingData))
                  , tagAlong = tagAlong parsingData
                }
              where
                newTables = init (symbolTables parsingData)
                oldTable = last (symbolTables parsingData)
                newVals = values (last (symbolTables parsingData)) ++ [scopeData]

addData :: ParsingData -> ScopeData -> ParsingData
addData parsingData scopeData
    = ParsingData { lookAheadToken=(lookAheadToken parsingData)
                  , hasFailed=(hasFailed parsingData)
                  , line=(line parsingData)
                  , column=(column parsingData)
                  , input=(input parsingData)
                  , symbolTables=(newTables ++ [SymbolTable { values=newVals }])
                  , current_lexeme=lexeme_scan(head (input parsingData))
                }
              where
                newTables = init (symbolTables parsingData)
                oldTable = last (symbolTables parsingData)
                endScopeData = last (values (last (symbolTables parsingData)))
                newVals = init (values (last (symbolTables parsingData))) ++ [newScope]
                  where
                    newScope = ScopeData  {name = name scopeData, kind = kind scopeData, varType = varType scopeData, attribute = attribute scopeData, offset = offset endScopeData} 
