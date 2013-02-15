-- Author: Murph Murphy
-- Date: 2/12/2013
-- These functions implement part of the grammar of micro-Pascal. They all take
-- in a ParsingData datatype that contains a lookAheadToken, and recursively
-- build an output string that it passes back in the parsingData. An error
-- during recursion indicates the syntax of the program was invalid.
--
-- Empty string is handled, every rule that could accept an empty string just
-- passes the data on without altering it if it's real rule doesn't match.
--
-- Errors are handled. Any rule that can error has an otherwise clause where
-- it calls the syntaxError helper function. Each function checks if an error
-- already has occured, and if it has, doesn't try to evaluate it's rule,
-- functionally breaking recursion there.
--
-- TODO: terminal handling.

import Parser.ParsingData
import Parser.Helper
import Scanner.TokenType

-- Handles a base Expression non-terminal.
expression :: ParsingData -> ParsingData
expression parsingData
    | hasFailed parsingData == True
        = parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = optionalRelationalPart (simpleExpression parsingData)
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = optionalRelationalPart (simpleExpression parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_NOT", "MP_LPAREN"]
        = optionalRelationalPart (simpleExpression parsingData)
    | otherwise
        = syntaxError (parsingData)

-- Handles the OptionalRelationalPart non-terminal.
optionalRelationalPart :: ParsingData -> ParsingData
optionalRelationalPart parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_EQUALS", "MP_LTHAN", "MP_GTHAN", "MP_LEQUAL", "MP_GTHAN", "MP_NEQUAL"]
        = simpleExpression (relationalOperator parsingData)
    | otherwise
        = parsingData -- empty string allowed

-- Handles the RelationalOperator terminal.
relationalOperator :: ParsingData -> ParsingData
relationalOperator parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_EQUALS", "MP_LTHAN", "MP_GTHAN", "MP_LEQUAL", "MP_GEQUAL", "MP_NEQUAL"]
        = terminal parsingData
    | otherwise
        = syntaxError (parsingData)

-- Handles the SimpleExpression non-terminal.
simpleExpression :: ParsingData -> ParsingData
simpleExpression parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS"]
        = termTail (term (optionalSign parsingData))
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = termTail (term (optionalSign parsingData))
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = termTail (term (optionalSign parsingData))
    | otherwise
        = parsingData -- empty string allowed

-- Handles the TermTail non-terminal.
termTail :: ParsingData -> ParsingData
termTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_OR"]
        = termTail (term (addingOperator parsingData))
    | otherwise
        = parsingData -- empty string allowed

-- Handles the OptionalSign terminal.
optionalSign :: ParsingData -> ParsingData
optionalSign parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS"]
        = terminal parsingData
    | otherwise
        = parsingData --empty string allowed

-- Handles the AddingOperator terminal.
addingOperator :: ParsingData -> ParsingData
addingOperator parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_OR"]
        = terminal parsingData
    | otherwise
        = syntaxError (parsingData)

term :: ParsingData -> ParsingData
term parsingData
    | hasFailed parsingData == True
        = parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = factorTail (factor parsingData)
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = factorTail (factor parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_NOT", "MP_LPAREN"]
        = factorTail (factor parsingData)
    | otherwise
        = syntaxError (parsingData)

-- Handles the FactorTail non-terminal.
factorTail :: ParsingData -> ParsingData
factorTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_TIMES", "MP_DIV", "MP_MOD", "MP_AND"]
        = factorTail (factor (multiplyingOperator parsingData))
    | otherwise
        = parsingData -- empty string allowed

-- Handels the multiplyingOperator terminal.
multiplyingOperator :: ParsingData -> ParsingData
multiplyingOperator parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_TIMES", "MP_DIV", "MP_MOD", "MP_AND"]
        = terminal parsingData
    | otherwise
        = syntaxError (parsingData)

-- Handles the Factor grammar rule.
factor :: ParsingData -> ParsingData
factor parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = variableIdentifier parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = optionalActualParemeterList (functionIdentifier parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER_LIT", "MP_FIXED_LIT", "MP_FLOAT_LIT"]
        = terminal parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_NOT"
        = factor (terminal parsingData)
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = terminal (expression (terminal parsingData))
    | otherwise
        = syntaxError (parsingData)

-- Handles the ProgramIdentifier terminal.
programIdentifier :: ParsingData -> ParsingData
programIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifier parsingData
    | otherwise
        = syntaxError (parsingData)

-- Handles the VariableIdentifier terminal.
variableIdentifier :: ParsingData -> ParsingData
variableIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifier parsingData
    | otherwise
        = syntaxError (parsingData)

-- Handles the ProcedureIdentifier terminal.
procedureIdentifier :: ParsingData -> ParsingData
procedureIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifier parsingData
    | otherwise
        = syntaxError (parsingData)

-- Handles the FunctionIdentifier terminal.
functionIdentifier :: ParsingData -> ParsingData
functionIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = identifier parsingData
    | otherwise
        = syntaxError (parsingData)

-- Handles the BooleanExpression non-terminal.
booleanExpression :: ParsingData -> ParsingData
booleanExpression parsingData
    | hasFailed parsingData == True
        = parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = expression parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = expression parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_NOT", "MP_LPAREN"]
        = expression parsingData
    | otherwise
        = syntaxError (parsingData)

ordinalExpression :: ParsingData -> ParsingData
ordinalExpression parsingData
    | hasFailed parsingData == True
        = parsingData
    | getTokenType (lookAheadToken parsingData) ==  "IdentifierOrLiteral"
        = expression parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = expression parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_NOT", "MP_LPAREN"]
        = expression parsingData
    | otherwise
        = syntaxError (parsingData)

identifierList :: ParsingData -> ParsingData
identifierList parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifierList (terminal identifier)
    | otherwise
        = syntaxError (parsingData)

identifierTail :: ParsingData -> ParsingData
identifierTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = identifierTail (identifier (terminal parsingData))
    | otherwise
        = parsingData -- empty string allowed
