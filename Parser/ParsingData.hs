module Parser.ParsingData where

import Scanner.TokenTable
import Scanner.ScannerData

data ParsingData = ParsingData {  lookAheadToken :: Token
                                , hasFailed :: Bool
                                , line :: Int
                                , column :: Int
                                , errorString :: String
                                , input :: [ScannerData]
                                } deriving (Show)
