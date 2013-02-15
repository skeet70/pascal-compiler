-- Author: James Sonntag
-- Date: 2/13/2013
-- These functions implement part of the grammar of micro-Pascal. They all take
-- in a ParsingData datatype that contains a lookAheadToken, and recursively
-- build an output string that it passes back in the parsingData. An error
-- during recursion indicates the syntax of the program was invalid.
--
-- TODO: Error handling, empty string/lambda, and terminal handling.
module Parser.SectionTwo where

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
    | hasFailed parsingData == True
        = parsingData
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
        = syntaxError parsingData

emptyStatement :: ParsingData -> ParsingData
emptyStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        syntaxError parsingData

--ReadStatement ⟶ "read" "(" ReadParameter ReadParameterTail ")"
readStatement :: ParsingData -> ParsingData
readStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) ==  "MP_READ"
        = terminal (readParameterTail (readParameter (terminal (terminal parsingData))))
    | otherwise
        = syntaxError parsingData

--ReadParameterTail ⟶ "," ReadParameter ReadParameterTail
--                  ⟶ ε
readParameterTail :: ParsingData -> ParsingData
readParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = readParameterTail (readParameter (terminal parsingData))
    | (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

--ReadParameter ⟶ VariableIdentifier
readParameter :: ParsingData -> ParsingData
readParameter parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = variableIdentifier parsingData
    | otherwise
        = syntaxError parsingData

--WriteStatement ⟶ "write" "(" WriteParameter WriteParameterTail ")"
writeStatement :: ParsingData -> ParsingData
writeStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WRITE"
        = terminal (writeParameterTail (writeParameter (terminal (terminal parsingData))))
    | otherwise
        = syntaxError parsingData

--WriteParameterTail  ⟶ "," WriteParameter
--                    ⟶ ε
writeParameterTail :: ParsingData -> ParsingData
writeParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = writeParameter (terminal parsingData)
    | (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

--WriteParameter ⟶ OrdinalExpression
writeParameter :: ParsingData -> ParsingData
writeParameter parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken (lookAheadToken parsingData)) ["MP_PLUS", "MP_MINUS"] || (lookAheadToken parsingData) == lambda
        = ordinalExpression parsingData
    | otherwise
        = syntaxError parsingData

--AssignmentStatement ⟶ VariableIdentifier ":=" Expression
--                    ⟶ FunctionIdentifier ":=" Expression 
assignmentStatement :: ParsingData -> ParsingData
assignmentStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = expression (terminal (variableIdentifier parsingData))
    | getTokenType (lookAheadToken parsingData) == "ReservedWord"
        = expression (terminal (functionIdentifier parsingData))  --Just for now, need to get clarification from Rocky
    | otherwise
        = syntaxError parsingData

--IfStatement ⟶ "if" BooleanExpression "then" Statement OptionalElsePart
ifStatement :: ParsingData -> ParsingData
ifStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IF"
        = optionalElsePart (statment (terminal (booleanExpression (terminal parsingData))))
    | otherwise
        = syntaxError parsingData

--OptionalElsePart ⟶ "else" Statement
--                 ⟶ ε  
optionalElsePart :: ParsingData -> ParsingData
optionalElsePart parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_ELSE"
        = statement (terminal parsingData)
    | (lookAheadToken parsingData) == lambda
        = parsingData
    |otherwise
        = syntaxError parsingData

--RepeatStatement ⟶ "repeat" StatementSequence "until" BooleanExpression  
repeatStatement :: ParsingData -> ParsingData
repeatStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_REPEAT"
        = booleanExpression (terminal (statementSequence (terminal parsingData)))
    | otherwise
        = syntaxError parsingData

--WhileStatement ⟶ "while" BooleanExpression "do" Statement  
whileStatement :: ParsingData -> ParsingData
whileStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WHILE"
        = statement (terminal (booleanExpression (terminal parsingData)))
    | otherwise
        = syntaxError parsingData

--ForStatement ⟶ "for" ControlVariable ":=" InitialValue StepValue FinalValue "do" Statement
forStatement :: ParsingData -> ParsingData
forStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FOR"
        = statement (terminal (finalValue (stepValue (initialValue (terminal (controlVariable (terminal parsingData)))))))
    | otherwise
        = syntaxError parsingData

--ControlVariable ⟶ VariableIdentifier
controlVariable :: ParsingData -> ParsingData
controlVariable parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = variableIdentifier parsingData
    | otherwise
        = syntaxError parsingData

--InitialValue ⟶ OrdinalExpression
initialValue :: ParsingData -> ParsingData
initialValue parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken (lookAheadToken parsingData)) ["MP_PLUS", "MP_MINUS"] || (lookAheadToken parsingData) == lambda
        = ordinalExpression parsingData
    | otherwise
        = syntaxError parsingData

--StepValue ⟶ "to"
--          ⟶ "downto"
stepValue :: ParsingData -> ParsingData
stopValue parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_TO"
        = terminal parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_DOWNTO"
        = terminal parsingData
    | otherwise
        = syntaxError parsingData

--FinalValue ⟶ OrdinalExpression
finalValue :: ParsingData -> ParsingData
finalValue parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken ((lookAheadToken parsingData))) ["MP_PLUS", "MP_MINUS"] || (lookAheadToken parsingData) == lambda
        = ordinalExpression parsingData
    | otherwise
        = syntaxError parsingData

--ProcedureStatement ⟶ ProcedureIdentifier OptionalActualParameterList
procedureStatement :: ParsingData -> ParsingData
procedureStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = optionalActualParameterList (procedureIdentifier parsingData)
    | otherwise
        = syntaxError parsingData

--OptionalActualParameterList ⟶ "(" ActualParameter ActualParameterTail ")"
--                            ⟶ ε
optionalActualParameterList :: ParsingData -> ParsingData
optionalActualParameterList parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = terminal (actualParameterTail (actualParameter (terminal parsingData)))
    | (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

--ActualParameterTail ⟶ "," ActualParameter ActualParameterTail
--                    ⟶ ε
actualParameterTail :: ParsingData -> ParsingData
actualParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = actualParameterTail (actualParameter (terminal parsingData))
    | (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

--ActualParameter ⟶ OrdinalExpression
actualParameter :: ParsingData -> ParsingData
actualParameter parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken ((lookAheadToken parsingData))) ["MP_PLUS", "MP_MINUS"] || (lookAheadToken parsingData) == lambda
        = ordinalExpression parsingData
    | otherwise
        = syntaxError parsingData