Term                    ⟶ Factor FactorTail
FactorTail              ⟶ MultiplyingOperator Factor FactorTail
                        ⟶ ε
MultiplyingOperator     ⟶ "*"
                        ⟶ "div"
                        ⟶ "mod"
                        ⟶ "and"

Factor                  ⟶ UnsignedInteger
                        ⟶ VariableIdentifier
                        ⟶ "not" Factor
                        ⟶ "(" Expression ")"
                        ⟶ FunctionIdentifier OptionalActualParameterList
ProgramIdentifier    ⟶ Identifier

VariableIdentifier   ⟶ Identifier

ProcedureIdentifier  ⟶ Identifier

FunctionIdentifier   ⟶ Identifier
BooleanExpression    ⟶ Expression

OrdinalExpression    ⟶ Expression
IdentifierList       ⟶ Identifier IdentifierTail

IdentifierTail       ⟶ "," Identifier IdentifierTail
                     ⟶ ε
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
    | getTokenType lookAheadToken parsingData ==  IdentifierOrLiteral
        = optionalRelationalPart (simpleExpression parsingData)
    | any (unwrapToken lookAheadToken parsingData ==)  (MP_PLUS, MP_MINUS, MP_NOT, MP_LPAREN)
        = optionalRelationalPart (simpleExpression parsingData)
    | otherwise
        = -- handle error

-- Handles the OptionalRelationalPart non-terminal.
optionalRelationalPart :: ParsingData -> ParsingData
optionalRelationalPart parsingData
    | any (unwrapToken lookAheadToken parsingData ==) (MP_EQUALS, MP_LTHAN, MP_GTHAN, MP_LEQUAL, MP_GTHAN, MP_NEQUAL)
        = simpleExpression (relationalOperator parsingData)
    | lookAheadToken parsingData == lambda
        = -- handle empty string
    | otherwise
        = -- handle error

-- Handles the RelationalOperator terminal.
relationalOperator :: ParsingData -> ParsingData
relationalOperator parsingData
    | any (unwrapToken lookAheadToken parsingData ==) (MP_EQUALS, MP_LTHAN, MP_GTHAN, MP_LEQUAL, MP_GEQUAL, MP_NEQUAL)
        = -- handle terminals
    | otherwise
        = -- handle error

-- Handles the SimpleExpression non-terminal.
simpleExpression :: ParsingData -> ParsingData
simpleExpression parsingData
    | any (unwrapToken lookAheadToken parsingData ==) (MP_PLUS, MP_MINUS)
        = termTail (term (optionalSign parsingData))
    | lookAheadToken parsingData == lambda
        = -- handle lambda
    | otherwise
        = -- handle error

-- Handles the TermTail non-terminal.
termTail :: ParsingData -> ParsingData
termTail parsingData
    | any (unwrapToken lookAheadToken parsingData ==) (MP_PLUS, MP_MINUS, MP_OR)
        = termTail (term (addingOperator parsingData))
    | lookAheadToken parsingData == lambda
        = -- handle lambda
    | otherwise
        = -- handle error

-- Handles the OptionalSign terminal.
optionalSign :: ParsingData -> ParsingData
optionalSign parsingData
    | any (unwrapToken lookAheadToken parsingData ==) (MP_PLUS, MP_MINUS)
        = -- handle terminal
    | lookAheadToken parsingData == lambda
        = -- handle lambda
    | otherwise
        = -- handle error

-- Handles the AddingOperator terminal.
addingOperator :: ParsingData -> ParsingData
addingOperator parsingData
    | any (unwrapToken lookAheadToken parsingData ==) (MP_PLUS, MP_MINUS, MP_OR)
        = -- handle terminals
    | otherwise
        = -- handle error

