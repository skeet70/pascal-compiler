module Parser.Helper where

import Parser.ParsingData
import Scanner.TokenTable
import Scanner.ScannerData


-- This is called any time a rule cannot successfully match the current token.
-- It returns the same ParsingData object with the hasFailed flag tripped.
syntaxError :: ParsingData -> ParsingData
syntaxError parsingData = ParsingData {   lookAheadToken=(lookAheadToken parsingData)
                                        , hasFailed=True
                                        , line=(line parsingData)
                                        , column=(column parsingData)
                                        , input=(input parsingData)
                                    }

-- Generic called whenever a terminal is encountered. Gets the next token from
-- the input list, as well as the corresponding line_scan and column_scan.
match :: ParsingData -> ParsingData
match parsingData = ParsingData {  lookAheadToken=(token (head (input parsingData)))
                                    , hasFailed=False
                                    , line=(line_scan (head (input parsingData)))
                                    , column=(column_scan (head (input parsingData)))
                                    , input=(tail (input parsingData))
                                }

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a )
r_paren_match :: ParsingData -> ParsingData
r_paren_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_RPAREN"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a (
l_paren_match :: ParsingData -> ParsingData
l_paren_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a :=
assignment_match :: ParsingData -> ParsingData
assignment_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_ASSIGN"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a then
then_match :: ParsingData -> ParsingData
then_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_THEN"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be an until
until_match :: ParsingData -> ParsingData
until_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_UNTIL"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a do
do_match :: ParsingData -> ParsingData
do_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_DO"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a ;
semic_match :: ParsingData -> ParsingData
semic_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_SCOLON"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a .
period_match :: ParsingData -> ParsingData
period_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PERIOD"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a :
colon_match :: ParsingData -> ParsingData
colon_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COLON"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be a "end"
end_match :: ParsingData -> ParsingData
end_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_END"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData

-- Specific matching case called for an unkown terminal at the end of a terminal
-- that should be an identifier
ident_match :: ParsingData -> ParsingData
ident_match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = ParsingData {   lookAheadToken=(token (head (input parsingData)))
                        , hasFailed=False
                        , line=(line_scan (head (input parsingData)))
                        , column=(column_scan (head (input parsingData)))
                        , input=(tail (input parsingData))
                    }
    | otherwise
        = syntaxError parsingData
