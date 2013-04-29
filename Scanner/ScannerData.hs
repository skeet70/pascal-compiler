module Scanner.ScannerData where

import Scanner.TokenTable

data ScannerData = ScannerData { token :: Token
                                ,line_scan :: Int
                                ,column_scan :: Int
                                ,lexeme_scan :: String
                            } deriving (Show)