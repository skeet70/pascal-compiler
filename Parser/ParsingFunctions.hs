--Three sections of parsing functions stapled together
--
--Authored by: Tyler J. Huffman, James Sonntag, and Murph "Ryan" Murphy




--First section of Parsing Functions
--
--Authored by: Tyler J. Huffman

module Parser.ParsingFunctions where

import Parser.ParsingData
import Parser.Helper
import Scanner.TokenType


systemGoal :: ParsingData -> ParsingData
systemGoal parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_EOF"
        = parsingData
    | otherwise
        = program parsingData

program :: ParsingData -> ParsingData
program parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROGRAM" 
        = terminal (block (terminal (programHeading parsingData)))
    | otherwise
        = syntaxError parsingData

programHeading :: ParsingData -> ParsingData
programHeading parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROGRAM"
        = programIdentifier (terminal parsingData)
    | otherwise
        = syntaxError parsingData

block :: ParsingData -> ParsingData
block parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = statementPart ( procedureAndFunctionDeclarationPart ( variableDeclarationPart parsingData))
    | otherwise
        = syntaxError parsingData

variableDeclarationPart :: ParsingData -> ParsingData
variableDeclarationPart parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = variableDeclarationTail ( terminal ( variableDeclaration ( terminal parsingData)))
    | otherwise
        = syntaxError parsingData

variableDeclarationTail :: ParsingData -> ParsingData
variableDeclarationTail parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = variableDeclarationTail ( terminal ( variableDeclaration parsingData))
    | lookAheadToken parsingData == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

variableDeclaration :: ParsingData -> ParsingData
variableDeclaration parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = typeParser ( terminal ( identifierList parsingData))
    | otherwise
        = syntaxError parsingData

typeParser :: ParsingData -> ParsingData
typeParser parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER", "MP_FLOAT", "MP_BOOLEAN"] 
        = terminal parsingData
    | otherwise
        = syntaxError parsingData

procedureAndFunctionDeclarationPart :: ParsingData -> ParsingData
procedureAndFunctionDeclarationPart parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = procedureAndFunctionDeclarationPart ( procedureDeclaration parsingData)
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION" 
        = procedureAndFunctionDeclarationPart ( functionDeclaration parsingData)
    | unwrapToken (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

procedureDeclaration :: ParsingData -> ParsingData
procedureDeclaration parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = terminal ( block ( terminal ( procedureHeading parsingData)))
    | otherwise
        = syntaxError parsingData

functionDeclaration :: ParsingData -> ParsingData
functionDeclaration parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = terminal ( block ( terminal ( functionHeading parsingData)))
    | otherwise
        = syntaxError parsingData

procedureHeading :: ParsingData -> ParsingData
procedureHeading parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = optionalFormalParameterList ( procedureIdentifier ( terminal parsingData))
    | otherwise
        = syntaxError parsingData

functionHeading :: ParsingData -> ParsingData
functionHeading parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = typeParser ( terminal ( optionalFormalParameterList ( functionIdentifier ( terminal parsingData))))
    | otherwise
        = syntaxError parsingData

optionalFormalParameterList :: ParsingData -> ParsingData
optionalFormalParameterList parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN" 
        = terminal ( formalParameterSectionTail ( formalParameterSection ( terminal parsingData)))
    | unwrapToken (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

formalParameterSectionTail :: ParsingData -> ParsingData
formalParameterSectionTail parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_SCOLON" 
        = formalParameterSectionTail ( formalParameterSection ( terminal parsingData))
    | unwrapToken (lookAheadToken parsingData) == lambda 
        = parsingData
    | otherwise
        = syntaxError parsingData

formalParameterSection :: ParsingData -> ParsingData
formalParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"  
        = valueParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"  
        = variableParameterSection
    | otherwise
        = syntaxError parsingData

valueParameterSection :: ParsingData -> ParsingData
valueParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"   
        = typeParser ( terminal ( identifierList parsingData))
    | otherwise
        = syntaxError parsingData

variableParameterSection :: ParsingData -> ParsingData
variableParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"   
        = typeParser ( terminal ( identifierList ( terminal parsingData)))
    | otherwise
        = syntaxError parsingData

statementPart :: ParsingData -> ParsingData
statementPart parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_BEGIN"
        = compoundStatement parsingData
    | otherwise
        = syntaxError parsingData

compoundStatement :: ParsingData -> ParsingData
compoundStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_BEGIN"
        = terminal ( statementSequence ( terminal parsingData))
    | otherwise
        = syntaxError parsingData

statementSequence :: ParsingData -> ParsingData
statementSequence parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_BEGIN", "MP_READ", "MP_WRITE", "MP_IDENTIFIER", "MP_IF", "MP_WHILE", "MP_REPEAT", "MP_FOR"]
        = statementTail ( statement ( parsingData))
    | otherwise
        = syntaxError parsingData

statementTail :: ParsingData -> ParsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_SCOLON"
        = statementTail ( statement ( terminal parsingData))
    | unwrapToken (lookAheadToken parsingData) == lambda
        = parsingData
    | otherwise
        = syntaxError parsingData

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

-- Author: James Sonntag
-- Date: 2/13/2013
-- These functions implement part of the grammar of micro-Pascal. They all take
-- in a ParsingData datatype that contains a lookAheadToken, and recursively
-- build an output string that it passes back in the parsingData. An error
-- during recursion indicates the syntax of the program was invalid.
--
-- TODO: Error handling, empty string/lambda, and terminal handling.


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
