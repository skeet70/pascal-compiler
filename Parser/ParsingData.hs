module Parser.ParsingData where

import Scanner.TokenTable
import Scanner.ScannerData

data ParsingData = ParsingData {  lookAheadToken :: Token
                                , hasFailed :: Bool
                                , line :: Int
                                , column :: Int
                                , input :: [ScannerData]
                                --otherStuff :: Token
                                --exampleStuff :: Bool
                                } deriving (Show)
