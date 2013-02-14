-- Author: James Sonntag
-- Date: 2/13/2013
-- These functions implement part of the grammar of micro-Pascal. They all take
-- in a ParsingData datatype that contains a lookAheadToken, and recursively
-- build an output string that it passes back in the parsingData. An error
-- during recursion indicates the syntax of the program was invalid.
--
-- TODO: Error handling, empty string/lambda, and terminal handling.

import Parser.ParsingData
import Scanner.TokenType


--Statement ⟶ EmptyStatement
--          ⟶ CompoundStatement
--          ⟶ ReadStatement
--          ⟶ WriteStatement
--          ⟶ AssignmentStatement
--          ⟶ IfStatement
--          ⟶ WhileStatement
--          ⟶ RepeatStatement
--          ⟶ ForStatement
--          ⟶ ProcedureStatement
statement :: ParsingData -> ParsingData
statement parsingData
    | (lookAheadToken parsingData) == lambda
        = emptyStatement parsingData
    | unwrapToken ((lookAheadToken parsingData)) ==  "MP_BEGIN"
        = compoundStatement parsingData
    | unwrapToken (lookAheadToken parsingData) ==  "MP_READ"
        = readStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WRITE"
        = writeStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = assignmentStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IF"
        = ifStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WHILE"
        = whileStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_REPEAT"
        = repeatStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FOR"
        = forStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = procedureStatement parsingData
    | otherwise
        = -- handle error

emptyStatement :: ParsingData -> ParsingData
emptyStatement parsingData
    | (lookAheadToken parsingData) == lambda
        = --handle lambda

--ReadStatement ⟶ "read" "(" ReadParameter ReadParameterTail ")"
readStatement :: ParsingData -> ParsingData
readStatement parsingData
    | unwrapToken (lookAheadToken parsingData) ==  "MP_READ"
        = terminal (readParameterTail (readParameter (terminal (terminal parsingData))))
    | otherwise
        = --handle error

--ReadParameterTail ⟶ "," ReadParameter ReadParameterTail
--                  ⟶ ε
readParameterTail :: ParsingData -> ParsingData
readParameterTail parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = readParameterTail (readParameter (terminal parsingData))
    | (lookAheadToken parsingData) == lambda
        = --handle lambda
    | otherwise
        = --handle error

--ReadParameter ⟶ VariableIdentifier
readParameter :: ParsingData -> ParsingData
readParameter parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = variableIdentifier parsingData
    | otherwise
        = --handle error

--WriteStatement ⟶ "write" "(" WriteParameter WriteParameterTail ")"
writeStatement :: ParsingData -> ParsingData
writeStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WRITE"
        = terminal (writeParameterTail (writeParameter (terminal (terminal parsingData))))
    | otherwise
        = --handle error

--WriteParameterTail  ⟶ "," WriteParameter
--                    ⟶ ε
writeParameterTail :: ParsingData -> ParsingData
writeParameterTail parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = writeParameter (terminal parsingData)
    | (lookAheadToken parsingData) == lambda
        = --handle lambda
    | otherwise
        = --handle error

--WriteParameter ⟶ OrdinalExpression
writeParameter :: ParsingData -> ParsingData
writeParameter parsingData
    | any (== unwrapToken (lookAheadToken parsingData)) ["MP_PLUS", "MP_MINUS"] || (lookAheadToken parsingData) == lambda
        = ordinalExpression parsingData
    | otherwise
        = --handle error

--AssignmentStatement ⟶ VariableIdentifier ":=" Expression
--                    ⟶ FunctionIdentifier ":=" Expression 
assignmentStatement :: ParsingData -> ParsingData
assignmentStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = expression (terminal (variableIdentifier parsingData))
    | getTokenType (lookAheadToken parsingData) == "ReservedWord"
        = expression (terminal (functionIdentifier parsingData))  --Just for now, need to get clarification from Rocky
    | otherwise
        = --handle error

--IfStatement ⟶ "if" BooleanExpression "then" Statement OptionalElsePart
ifStatement :: ParsingData -> ParsingData
ifStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IF"
        = optionalElsePart (statment (terminal (booleanExpression (terminal parsingData))))
    | otherwise
        = --handle error