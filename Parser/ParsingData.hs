module Parser.ParsingData where

import Scanner.TokenTable
import Scanner.ScannerData

data ParsingData = ParsingData {  lookAheadToken :: Token
                                , hasFailed :: Bool
                                , line :: Int
                                , column :: Int
                                , errorString :: String
                                , input :: [ScannerData]
                                , symbolTables :: [SymbolTable]
                                , current_lexeme :: String
                                , tagAlong :: [String]
                                , intermediateCode :: [String] -- Need to add this to any replication.
                                } deriving (Show)

data SymbolTable = SymbolTable {  values :: [ScopeData] } deriving (Show)

data ScopeData = ScopeData {  name :: String
                            , kind :: String
                            , varType :: String
                            , level :: Int
                            , offset :: Int
                            } deriving (Show)

-- Creates an empty SymbolTable and tacks it onto the end of a ParsingData's
-- list.
createSymbolTable :: ParsingData -> ParsingData
createSymbolTable parsingData
    = ParsingData { lookAheadToken=(lookAheadToken parsingData)
                  , hasFailed=(hasFailed parsingData)
                  , line=(line parsingData)
                  , column=(column parsingData)
                  , input=(input parsingData)
                  , symbolTables=(symbolTables parsingData ++ [SymbolTable { values=[] }])
                  , current_lexeme=lexeme_scan(head (input parsingData))
                  , tagAlong = tagAlong parsingData
                }

-- Destroys the most recent SymbolTable in a given ParsingData's list of tables.
destroySymbolTable :: ParsingData -> ParsingData
destroySymbolTable parsingData
    = ParsingData { lookAheadToken=(lookAheadToken parsingData)
                  , hasFailed=(hasFailed parsingData)
                  , line=(line parsingData)
                  , column=(column parsingData)
                  , input=(input parsingData)
                  , symbolTables=(init (symbolTables parsingData))
                  , current_lexeme=lexeme_scan(head (input parsingData))
                  , intermediateCode = intermediateCode parsingData
                  , tagAlong = tagAlong parsingData
                }

-- Searches through the SymbolTables of a given ParsingData for a lexeme and
-- returns the relevant ScopeData. If it can't find it, it will return a
-- ScopeData with kind = None.
searchSymbolTables :: ParsingData -> String -> ScopeData
searchSymbolTables parsingData lexeme
    | not (null (symbolTables parsingData)) && kind result /= "None"
        = result
    | not (null (symbolTables parsingData)) && kind result == "None"
        = searchSymbolTables newParsingData lexeme
    | otherwise
        = ScopeData { kind = "None" }
      where
        result = walkTable (last (symbolTables parsingData)) (length (symbolTables parsingData) - 1) lexeme
        newParsingData = ParsingData { lookAheadToken=(lookAheadToken parsingData)
                                     , hasFailed=(hasFailed parsingData)
                                     , line=(line parsingData)
                                     , column=(column parsingData)
                                     , input=(input parsingData)
                                     , symbolTables=init (symbolTables parsingData)
                                     , current_lexeme=(current_lexeme parsingData)
                                     , intermediateCode = intermediateCode parsingData
                                     , tagAlong = tagAlong parsingData
                                }

-- Utility function. Walks through a given table's tuples and returns the
-- matching ScopeData. If it isn't there, it will return a ScopeData of the kind
-- None
walkTable :: SymbolTable -> Int -> String -> ScopeData
walkTable symbolTable index lexeme
    | name scopeData == lexeme
        = ScopeData { name = name scopeData
                    , kind = kind scopeData
                    , varType = varType scopeData
                    , level = index
                    , offset = offset scopeData
        }
    | not $ null $ values symbolTable
        = walkTable newSymbolTable index lexeme
    | otherwise
        = ScopeData { kind = "None"}
      where
        scopeData = head $ values symbolTable
        newSymbolTable = SymbolTable { values = tail $ values symbolTable }

-- Inserts the given ScopeData into the most recent SymbolTable.
insertData :: ParsingData -> ScopeData -> ParsingData
insertData parsingData scopeData
    =  ParsingData { lookAheadToken=(lookAheadToken parsingData)
                  , hasFailed=(hasFailed parsingData)
                  , line=(line parsingData)
                  , column=(column parsingData)
                  , input=(input parsingData)
                  , symbolTables=(newTables ++ [SymbolTable { values=newVals }])
                  , current_lexeme=lexeme_scan(head (input parsingData))
                  , intermediateCode = intermediateCode parsingData
                  , tagAlong = tagAlong parsingData
                }
              where
                newTables = init (symbolTables parsingData)
                oldTable = last (symbolTables parsingData)
                newVals = values (last (symbolTables parsingData)) ++ [scopeData]
