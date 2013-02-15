module Parser.Helper where

import Parser.ParsingData

-- This is called any time a rule cannot successfully match the current token.
-- It returns the same ParsingData object with the hasFailed flag tripped.
syntaxError :: ParsingData -> ParsingData
syntaxError parsingData = ParsingData {   lookAheadToken=(lookAheadToken parsingData)
                                        , hasFailed=True
                                        , line=(line parsingData)
                                        , column=(column parsingData)
                                    }

-- Called whenever a terminal is encountered. Gets the next token from the input
-- list, as well as the corresponding line and column.
terminal :: ParsingData -> ParsingData
terminal parsingData = ParsingData {  lookAheadToken=(token (head (input parsingData)))
                                    , hasFailed=False
                                    , line=(line (head (input parsingData)))
                                    , column=(column (head (input parsingData)))
                                }
