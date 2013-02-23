--Three sections of parsing functions stapled together
--
--Authored by: Tyler J. Huffman, James Sonntag, and Murph "Ryan" Murphy

--Edited: Feb. 19, 2013


--First section of Parsing Functions
--
--Authored by: Tyler J. Huffman

module Parser.ParsingFunctions where

import Debug.Trace

import Parser.ParsingData
import Parser.Helper
import Scanner.TokenTable

-- SystemGoal ⟶ Program eof
systemGoal :: ParsingData -> ParsingData
systemGoal parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROGRAM"
        = eof_match (program parsingData)
    | otherwise
        = syntaxError "MP_PROGRAM" parsingData

-- Program ⟶ ProgramHeading ";" Block "."
program :: ParsingData -> ParsingData
program parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROGRAM"
        = period_match (block (semic_match (programHeading parsingData)))
    | otherwise
        = syntaxError "MP_PROGRAM" parsingData

-- ProgramHeading ⟶ "program" ProgramIdentifier
programHeading :: ParsingData -> ParsingData
programHeading parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROGRAM"
        = programIdentifier (match parsingData)
    | otherwise
        = syntaxError "MP_PROGRAM" parsingData

-- Block ⟶ VariableDeclarationPart ProcedureAndFunctionDeclarationPart StatementPart
block :: ParsingData -> ParsingData
block parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = statementPart ( procedureAndFunctionDeclarationPart ( variableDeclarationPart parsingData))
    | otherwise
        = syntaxError "MP_VAR" parsingData

-- VariableDeclarationPart ⟶ "var" VariableDeclaration ";" VariableDeclarationTail
variableDeclarationPart :: ParsingData -> ParsingData
variableDeclarationPart parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = variableDeclarationTail ( semic_match ( variableDeclaration ( match parsingData)))
    | otherwise
        = syntaxError "MP_VAR" parsingData

-- VariableDeclarationTail ⟶ VariableDeclaration ";" VariableDeclarationTail
--                         ⟶ ε
variableDeclarationTail :: ParsingData -> ParsingData
variableDeclarationTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = variableDeclarationTail ( semic_match ( variableDeclaration parsingData))
    | otherwise
        = parsingData

-- VariableDeclaration ⟶ Identifierlist ":" Type
variableDeclaration :: ParsingData -> ParsingData
variableDeclaration parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = typeParser ( colon_match ( identifierList parsingData))
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

-- Type ⟶ "Integer"
--      ⟶ "Float"
--      ⟶ "Boolean"
typeParser :: ParsingData -> ParsingData
typeParser parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER", "MP_FLOAT", "MP_BOOLEAN"]
        = match parsingData
    | otherwise
        = syntaxError "MP_INTEGER, MP_FLOAT, or MP_BOOLEAN" parsingData

-- ProcedureAndFunctionDeclarationPart ⟶ ProcedureDeclaration ProcedureAndFunctionDeclarationPart
--                                     ⟶ FunctionDeclaration ProcedureAndFunctionDeclarationPart
--                                     ⟶ ε
procedureAndFunctionDeclarationPart :: ParsingData -> ParsingData
procedureAndFunctionDeclarationPart parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = procedureAndFunctionDeclarationPart ( procedureDeclaration parsingData)
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = procedureAndFunctionDeclarationPart ( functionDeclaration parsingData)
    | otherwise
        = parsingData

-- ProcedureDeclaration ⟶ ProcedureHeading ";" Block ";"
procedureDeclaration :: ParsingData -> ParsingData
procedureDeclaration parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = semic_match ( block ( semic_match ( procedureHeading parsingData)))
    | otherwise
        = syntaxError "MP_PROCEDURE" parsingData

-- FunctionDeclaration ⟶ FunctionHeading ";" Block ";"
functionDeclaration :: ParsingData -> ParsingData
functionDeclaration parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = semic_match ( block ( semic_match ( functionHeading parsingData)))
    | otherwise
        = syntaxError "MP_FUNCTION" parsingData

-- ProcedureHeading ⟶ "procedure" procedureIdentifier OptionalFormalParameterList
procedureHeading :: ParsingData -> ParsingData
procedureHeading parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = optionalFormalParameterList ( procedureIdentifier ( match parsingData))
    | otherwise
        = syntaxError "MP_PROCEDURE" parsingData

-- FunctionHeading ⟶ "function" functionIdentifier OptionalFormalParameterList ":" Type
functionHeading :: ParsingData -> ParsingData
functionHeading parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = typeParser ( colon_match ( optionalFormalParameterList ( functionIdentifier ( match parsingData))))
    | otherwise
        = syntaxError "MP_FUNCTION" parsingData

-- OptionalFormalParameterList ⟶ "(" FormalParameterSection FormalParameterSectionTail ")"
--                             ⟶ ε
optionalFormalParameterList :: ParsingData -> ParsingData
optionalFormalParameterList parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = r_paren_match ( formalParameterSectionTail ( formalParameterSection ( match parsingData)))
    | otherwise
        = parsingData

-- FormalParameterSectionTail ⟶ ";" FormalParameterSection FormalParameterSectionTail
--                            ⟶ ε
formalParameterSectionTail :: ParsingData -> ParsingData
formalParameterSectionTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_SCOLON"
        = formalParameterSectionTail ( formalParameterSection ( match parsingData))
    | otherwise
        = parsingData

-- FormalParameterSection ⟶ ValueParameterSection
--                        ⟶ VariableParameterSection
formalParameterSection :: ParsingData -> ParsingData
formalParameterSection parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = valueParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = variableParameterSection parsingData
    | otherwise
        = syntaxError "MP_IDENTIFIER or MP_VAR" parsingData

-- ValueParameterSection ⟶ IdentifierList ":" Type
valueParameterSection :: ParsingData -> ParsingData
valueParameterSection parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = typeParser ( colon_match ( identifierList parsingData))
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

-- VariableParameterSection ⟶ "var" IdentifierList ":" Type
variableParameterSection :: ParsingData -> ParsingData
variableParameterSection parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = typeParser ( colon_match ( identifierList ( match parsingData)))
    | otherwise
        = syntaxError "MP_VAR" parsingData

-- StatementPart ⟶ CompoundStatement
statementPart :: ParsingData -> ParsingData
statementPart parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_BEGIN"
        = compoundStatement parsingData
    | otherwise
        = syntaxError "MP_BEGIN" parsingData

-- CompoundStatement ⟶ "begin" StatementSequence "end"
compoundStatement :: ParsingData -> ParsingData
compoundStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_BEGIN"
        = end_match ( statementSequence ( match parsingData))
    | otherwise
        = syntaxError "MP_BEGIN" parsingData

-- StatementSequence ⟶ Statement StatementTail
statementSequence :: ParsingData -> ParsingData
statementSequence parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_BEGIN", "MP_READ", "MP_WRITE", "MP_IDENTIFIER", "MP_IF", "MP_WHILE", "MP_REPEAT", "MP_FOR"]
        = statementTail ( statement ( parsingData))
    | otherwise
        = syntaxError "MP_BEGIN, MP_READ, MP_WRITE, MP_IDENTIFIER, MP_IF, MP_WHILE, MP_REPEAT, or MP_FOR" parsingData

-- StatementTail ⟶ ";" Statement StatementTail
--               ⟶ ε
statementTail :: ParsingData -> ParsingData
statementTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_SCOLON"
        = statementTail ( statement ( match parsingData))
    | otherwise
        = parsingData

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

-- Expression              ⟶ SimpleExpression OptionalRelationalPart
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
        = syntaxError "IdentifierOrLiteral, ReservedWord, MP_PLUS, MP_MINUS, MP_NOT, or MP_LPAREN" parsingData

-- OptionalRelationalPart  ⟶ RelationalOperator SimpleExpression
--                         ⟶ ε
optionalRelationalPart :: ParsingData -> ParsingData
optionalRelationalPart parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_EQUALS", "MP_LTHAN", "MP_GTHAN", "MP_LEQUAL", "MP_GTHAN", "MP_NEQUAL"]
        = simpleExpression (relationalOperator parsingData)
    | otherwise
        = parsingData -- empty string allowed

-- RelationalOperator      ⟶ "="
--                         ⟶ "<"
--                         ⟶ ">"
--                         ⟶ "<="
--                         ⟶ ">="
--                         ⟶ "<>"
relationalOperator :: ParsingData -> ParsingData
relationalOperator parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_EQUALS", "MP_LTHAN", "MP_GTHAN", "MP_LEQUAL", "MP_GEQUAL", "MP_NEQUAL"]
        = match parsingData
    | otherwise
        = syntaxError "MP_EQUALS, MP_LTHAN, MP_GTHAN, MP_LEQUAL, MP_GEQUAL, or MP_NEQUAL" parsingData

-- SimpleExpression        ⟶ OptionalSign Term TermTail
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

-- TermTail                ⟶ AddingOperator Term TermTail
--                         ⟶ ε
termTail :: ParsingData -> ParsingData
termTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_OR"]
        = termTail (term (addingOperator parsingData))
    | otherwise
        = parsingData -- empty string allowed

-- OptionalSign            ⟶ "+"
--                         ⟶ "-"
--                         ⟶ ε
optionalSign :: ParsingData -> ParsingData
optionalSign parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS"]
        = match parsingData
    | otherwise
        = parsingData --empty string allowed

-- AddingOperator          ⟶ "+"
--                         ⟶ "-"
--                         ⟶ "or"
addingOperator :: ParsingData -> ParsingData
addingOperator parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_PLUS", "MP_MINUS", "MP_OR"]
        = match parsingData
    | otherwise
        = syntaxError "MP_PLUS, MP_MINUS, or MP_OR" parsingData

-- Term                    ⟶ Factor FactorTail
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
        = syntaxError "IdentifierOrLiteral, ReservedWord, MP_NOT, or MP_LPAREN" parsingData

-- FactorTail              ⟶ MultiplyingOperator Factor FactorTail
--                         ⟶ ε
factorTail :: ParsingData -> ParsingData
factorTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_TIMES", "MP_DIV", "MP_MOD", "MP_AND"]
        = factorTail (factor (multiplyingOperator parsingData))
    | otherwise
        = parsingData -- empty string allowed

-- MultiplyingOperator     ⟶ "*"
--                         ⟶ "div"
--                         ⟶ "mod"
--                         ⟶ "and"
multiplyingOperator :: ParsingData -> ParsingData
multiplyingOperator parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_TIMES", "MP_DIV", "MP_MOD", "MP_AND"]
        = match parsingData
    | otherwise
        = syntaxError "MP_TIMES, MP_DIV, MP_MOD, or MP_AND" parsingData

-- Factor                  ⟶ UnsignedInteger
--                         ⟶ "not" Factor
--                         ⟶ "(" Expression ")"
--                         ⟶ FunctionIdentifier OptionalActualParameterList
factor :: ParsingData -> ParsingData
factor parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) ==  "MP_IDENTIFIER"
        = optionalActualParameterList (functionIdentifier parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER_LIT", "MP_FIXED_LIT", "MP_FLOAT_LIT"]
        = match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_NOT"
        = factor (match parsingData)
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = r_paren_match (expression (match parsingData))
    | otherwise
        = syntaxError "MP_IDENTIFIER, MP_INTEGER_LIT, MP_FIXED_LIT, MP_FLOAT_LIT, MP_NOT, or MP_LPAREN" parsingData

-- ProgramIdentifier       ⟶ Identifier
programIdentifier :: ParsingData -> ParsingData
programIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = match parsingData
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

-- ProcedureIdentifier     ⟶ Identifier
procedureIdentifier :: ParsingData -> ParsingData
procedureIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = match parsingData
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

-- FunctionIdentifier      ⟶ Identifier
functionIdentifier :: ParsingData -> ParsingData
functionIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) ==  "MP_IDENTIFIER"
        = match parsingData
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

-- BooleanExpression       ⟶ Expression
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
        = syntaxError "IdentifierOrLiteral, ReservedWord, MP_PLUS, MP_MINUS, MP_NOT, or MP_LPAREN" parsingData

-- OrdinalExpression       ⟶ Expression
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
        = syntaxError "IdentifierOrLiteral, ReservedWord, MP_PLUS, MP_MINUS, MP_NOT, or MP_LPAREN"parsingData

-- IdentifierList          ⟶ Identifier IdentifierTail
identifierList :: ParsingData -> ParsingData
identifierList parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) =="MP_IDENTIFIER"
        = identifierList (match parsingData)
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

-- IdentifierTail          ⟶ "," Identifier IdentifierTail
--                         ⟶ ε
identifierTail :: ParsingData -> ParsingData
identifierTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = identifierTail (ident_match (match parsingData))
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
        = emptyStatement parsingData

emptyStatement :: ParsingData -> ParsingData
emptyStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | otherwise
        = parsingData

--ReadStatement ⟶ "read" "(" ReadParameter ReadParameterTail ")"
readStatement :: ParsingData -> ParsingData
readStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) ==  "MP_READ"
        = r_paren_match (readParameterTail (readParameter (l_paren_match (match parsingData))))
    | otherwise
        = syntaxError "MP_READ" parsingData

--ReadParameterTail ⟶ "," ReadParameter ReadParameterTail
--                  ⟶ ε
readParameterTail :: ParsingData -> ParsingData
readParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = readParameterTail (readParameter (match parsingData))
    | otherwise
        = parsingData

--ReadParameter ⟶ FunctionIdentifier
readParameter :: ParsingData -> ParsingData
readParameter parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = functionIdentifier parsingData
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

--WriteStatement ⟶ "write" "(" WriteParameter WriteParameterTail ")"
writeStatement :: ParsingData -> ParsingData
writeStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WRITE"
        = r_paren_match (writeParameterTail (writeParameter (l_paren_match (match parsingData))))
    | otherwise
        = syntaxError "MP_WRITE" parsingData

--WriteParameterTail  ⟶ "," WriteParameter
--                    ⟶ ε
writeParameterTail :: ParsingData -> ParsingData
writeParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = writeParameter (match parsingData)
    | otherwise
        = parsingData

--WriteParameter ⟶ OrdinalExpression
writeParameter :: ParsingData -> ParsingData
writeParameter parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken (lookAheadToken parsingData)) ["MP_PLUS", "MP_MINUS"]
        = ordinalExpression parsingData
    | otherwise
        = syntaxError "MP_PLUS or MP_MINUS" parsingData

--AssignmentStatement ⟶ FunctionIdentifier ":=" Expression
assignmentStatement :: ParsingData -> ParsingData
assignmentStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = expression (assignment_match (functionIdentifier parsingData))
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

--IfStatement ⟶ "if" BooleanExpression "then" Statement OptionalElsePart
ifStatement :: ParsingData -> ParsingData
ifStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IF"
        = optionalElsePart (statement (then_match (booleanExpression (match parsingData))))
    | otherwise
        = syntaxError "MP_IF" parsingData

--OptionalElsePart ⟶ "else" Statement
--                 ⟶ ε
optionalElsePart :: ParsingData -> ParsingData
optionalElsePart parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_ELSE"
        = statement (match parsingData)
    |otherwise
        = parsingData

--RepeatStatement ⟶ "repeat" StatementSequence "until" BooleanExpression
repeatStatement :: ParsingData -> ParsingData
repeatStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_REPEAT"
        = booleanExpression (until_match (statementSequence (match parsingData)))
    | otherwise
        = syntaxError "MP_REPEAT" parsingData

--WhileStatement ⟶ "while" BooleanExpression "do" Statement
whileStatement :: ParsingData -> ParsingData
whileStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WHILE"
        = statement (do_match (booleanExpression (match parsingData)))
    | otherwise
        = syntaxError "MP_WHILE" parsingData

--ForStatement ⟶ "for" ControlVariable ":=" InitialValue StepValue FinalValue "do" Statement
forStatement :: ParsingData -> ParsingData
forStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FOR"
        = statement (do_match (finalValue (stepValue (initialValue (assignment_match (controlVariable (match parsingData)))))))
    | otherwise
        = syntaxError "MP_FOR" parsingData

--ControlVariable ⟶ FunctionIdentifier
controlVariable :: ParsingData -> ParsingData
controlVariable parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = functionIdentifier parsingData
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

--InitialValue ⟶ OrdinalExpression
initialValue :: ParsingData -> ParsingData
initialValue parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken (lookAheadToken parsingData)) ["MP_PLUS", "MP_MINUS"]
        = ordinalExpression parsingData
    | otherwise
        = syntaxError "MP_PLUS or MP_MINUS" parsingData

--StepValue ⟶ "to"
--          ⟶ "downto"
stepValue :: ParsingData -> ParsingData
stepValue parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_TO"
        = match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_DOWNTO"
        = match parsingData
    | otherwise
        = syntaxError "MP_TO or MP_DOWNTO" parsingData

--FinalValue ⟶ OrdinalExpression
finalValue :: ParsingData -> ParsingData
finalValue parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken ((lookAheadToken parsingData))) ["MP_PLUS", "MP_MINUS"]
        = ordinalExpression parsingData
    | otherwise
        = syntaxError "MP_PLUS or MP_MINUS" parsingData

--ProcedureStatement ⟶ ProcedureIdentifier OptionalActualParameterList
procedureStatement :: ParsingData -> ParsingData
procedureStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = optionalActualParameterList (procedureIdentifier parsingData)
    | otherwise
        = syntaxError "MP_IDENTIFIER" parsingData

--OptionalActualParameterList ⟶ "(" ActualParameter ActualParameterTail ")"
--                            ⟶ ε
optionalActualParameterList :: ParsingData -> ParsingData
optionalActualParameterList parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = r_paren_match (actualParameterTail (actualParameter (match parsingData)))
    | otherwise
        = parsingData

--ActualParameterTail ⟶ "," ActualParameter ActualParameterTail
--                    ⟶ ε
actualParameterTail :: ParsingData -> ParsingData
actualParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = actualParameterTail (actualParameter (match parsingData))
    | otherwise
        = parsingData

--ActualParameter ⟶ OrdinalExpression
actualParameter :: ParsingData -> ParsingData
actualParameter parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (== unwrapToken ((lookAheadToken parsingData))) ["MP_PLUS", "MP_MINUS"]
        = ordinalExpression parsingData
    | otherwise
        = syntaxError "MP_PLUS or MP_MINUS" parsingData