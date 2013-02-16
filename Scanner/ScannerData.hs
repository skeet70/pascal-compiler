module Scanner.ScannerData where

import Scanner.TokenTable

data ScannerData = ScannerData { token :: Token
                                ,line_scan :: Int
                                ,column_scan :: Int
                            } deriving (Show)