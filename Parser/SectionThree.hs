-- Author: Murph Murphy
-- Date: 2/12/2013
-- These functions implement part of the grammar of micro-Pascal. They all take
-- in a ParsingData datatype that contains a lookAheadToken, and recursively
-- build an output string that it passes back in the parsingData. An error
-- during recursion indicates the syntax of the program was invalid.
--
-- TODO: Error handling, empty string/lambda, and terminal handling.

import Parser.ParsingData
import Scanner.TokenType

-- Handles a base Expression non-terminal.
expression :: ParsingData -> ParsingData
expression parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = optionalRelationalPart (simpleExpression parsingData)
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = optionalRelationalPart (simpleExpression parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_NOT", "MP_LPAREN"]
        = optionalRelationalPart (simpleExpression parsingData)
    | otherwise
        = -- handle error

-- Handles the OptionalRelationalPart non-terminal.
optionalRelationalPart :: ParsingData -> ParsingData
optionalRelationalPart parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_EQUALS", "MP_LTHAN", "MP_GTHAN", "MP_LEQUAL", "MP_GTHAN", "MP_NEQUAL"]
        = simpleExpression (relationalOperator parsingData)
    | lookAheadToken parsingData == lambda
        = -- handle empty string
    | otherwise
        = -- handle error

-- Handles the RelationalOperator terminal.
relationalOperator :: ParsingData -> ParsingData
relationalOperator parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_EQUALS", "MP_LTHAN", "MP_GTHAN", "MP_LEQUAL", "MP_GEQUAL", "MP_NEQUAL"]
        = -- handle terminals
    | otherwise
        = -- handle error

-- Handles the SimpleExpression non-terminal.
simpleExpression :: ParsingData -> ParsingData
simpleExpression parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS"]
        = termTail (term (optionalSign parsingData))
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = termTail (term (optionalSign parsingData))
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = termTail (term (optionalSign parsingData))
    | lookAheadToken parsingData == lambda
        = -- handle lambda
    | otherwise
        = -- handle error

-- Handles the TermTail non-terminal.
termTail :: ParsingData -> ParsingData
termTail parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_OR"]
        = termTail (term (addingOperator parsingData))
    | lookAheadToken parsingData == lambda
        = -- handle lambda
    | otherwise
        = -- handle error

-- Handles the OptionalSign terminal.
optionalSign :: ParsingData -> ParsingData
optionalSign parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS"]
        = -- handle terminal
    | lookAheadToken parsingData == lambda
        = -- handle lambda
    | otherwise
        = -- handle error

-- Handles the AddingOperator terminal.
addingOperator :: ParsingData -> ParsingData
addingOperator parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_OR"]
        = -- handle terminals
    | otherwise
        = -- handle error

term :: ParsingData -> ParsingData
term parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = factorTail (factor parsingData)
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = factorTail (factor parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_NOT", "MP_LPAREN"]
        = factorTail (factor parsingData)
    | otherwise
        = -- handle error

-- Handles the FactorTail non-terminal.
factorTail :: ParsingData -> ParsingData
factorTail parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_TIMES", "MP_DIV", "MP_MOD", "MP_AND"]
        = factorTail (factor (multiplyingOperator parsingData))
    | lookAheadToken parsingData == lambda
        = -- handle lambda
    | otherwise
        = -- handle error

-- Handels the multiplyingOperator terminal.
multiplyingOperator :: ParsingData -> ParsingData
multiplyingOperator parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_TIMES", "MP_DIV", "MP_MOD", "MP_AND"]
        = -- handle terminals
    | otherwise
        = -- handle error

-- Handles the Factor grammar rule.
factor :: ParsingData -> ParsingData
factor parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = variableIdentifier parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = optionalActualParemeterList (functionIdentifier parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER_LIT", "MP_FIXED_LIT", "MP_FLOAT_LIT"]
        = -- handle terminal
    | unwrapToken (lookAheadToken parsingData) == "MP_NOT"
        = factor (terminal parsingData) -- handle terminal
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = terminal (expression (terminal parsingData)) -- handle terminal
    | otherwise
        = -- handle error

-- Handles the ProgramIdentifier terminal.
programIdentifier :: ParsingData -> ParsingData
programIdentifier parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifier parsingData
    | otherwise
        = -- handle error

-- Handles the VariableIdentifier terminal.
variableIdentifier :: ParsingData -> ParsingData
variableIdentifier parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifier parsingData
    | otherwise
        = -- handle error

-- Handles the ProcedureIdentifier terminal.
procedureIdentifier :: ParsingData -> ParsingData
procedureIdentifier parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifier parsingData
    | otherwise
        = -- handle error

-- Handles the FunctionIdentifier terminal.
functionIdentifier :: ParsingData -> ParsingData
functionIdentifier parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = identifier parsingData
    | otherwise
        = -- handle error

-- Handles the BooleanExpression non-terminal.
booleanExpression :: ParsingData -> ParsingData
booleanExpression parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = expression parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = expression parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_NOT", "MP_LPAREN"]
        = expression parsingData
    | otherwise
        = -- handle error

ordinalExpression :: ParsingData -> ParsingData
ordinalExpression parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = expression parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = expression parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_NOT", "MP_LPAREN"]
        = expression parsingData
    | otherwise
        = -- handle error

identifierList :: ParsingData -> ParsingData
identifierList parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifierList (terminal identifier) -- handle terminal
    | otherwise
        = -- handle error

identifierTail :: ParsingData -> ParsingData
identifierTail parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = identifierTail (identifier (terminal parsingData)) -- handle terminal
    | lookAheadToken parsingData == lambda
        = -- handle empty string
    | otherwise
        = -- handle error
