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
                                , logging :: [String]
                                } deriving (Show)

data SymbolTable = SymbolTable {  values :: [ScopeData] }

data ScopeData = ScopeData {  name :: String
                            , kind :: String
                            , varType :: String
                            , attribute :: String
                            , offset :: Int
                            } deriving (Show)
