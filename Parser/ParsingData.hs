data ParsingData = ParsingData {  lookAheadToken :: Token
                                , hasFailed :: Bool
                                , line :: Int
                                , column :: Int
                                , input :: List
                                --otherStuff :: Token
                                --exampleStuff :: Bool
                                } deriving (Show)
