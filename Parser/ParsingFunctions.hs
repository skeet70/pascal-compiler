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

-- SystemGoal ⟶ Program eof
systemGoal :: ParsingData -> ParsingData
systemGoal parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_EOF"
        = parsingData
    | otherwise
        = program parsingData

-- Program ⟶ ProgramHeading ";" Block "."
program :: ParsingData -> ParsingData
program parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROGRAM"
        = period_match (block (semic_match (programHeading parsingData)))
    | otherwise
        = syntaxError parsingData

-- ProgramHeading ⟶ "program" ProgramIdentifier
programHeading :: ParsingData -> ParsingData
programHeading parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROGRAM"
        = programIdentifier (match parsingData)
    | otherwise
        = syntaxError parsingData

-- Block ⟶ VariableDeclarationPart ProcedureAndFunctionDeclarationPart StatementPart
block :: ParsingData -> ParsingData
block parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = statementPart ( procedureAndFunctionDeclarationPart ( variableDeclarationPart parsingData))
    | otherwise
        = syntaxError parsingData

-- VariableDeclarationPart ⟶ "var" VariableDeclaration ";" VariableDeclarationTail
variableDeclarationPart :: ParsingData -> ParsingData
variableDeclarationPart parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = variableDeclarationTail ( semic_match ( variableDeclaration ( match parsingData)))
    | otherwise
        = syntaxError parsingData

-- VariableDeclarationTail ⟶ VariableDeclaration ";" VariableDeclarationTail
--                         ⟶ ε
variableDeclarationTail :: ParsingData -> ParsingData
variableDeclarationTail parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = variableDeclarationTail ( semic_match ( variableDeclaration parsingData))
    | otherwise
        = parsingData

-- VariableDeclaration ⟶ Identifierlist ":" Type
variableDeclaration :: ParsingData -> ParsingData
variableDeclaration parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = typeParser ( colon_match ( identifierList parsingData))
    | otherwise
        = syntaxError parsingData

-- Type ⟶ "Integer"
--      ⟶ "Float"
--      ⟶ "Boolean"
typeParser :: ParsingData -> ParsingData
typeParser parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER", "MP_FLOAT", "MP_BOOLEAN"]
        = match parsingData
    | otherwise
        = syntaxError parsingData

-- ProcedureAndFunctionDeclarationPart ⟶ ProcedureDeclaration ProcedureAndFunctionDeclarationPart
--                                     ⟶ FunctionDeclaration ProcedureAndFunctionDeclarationPart
--                                     ⟶ ε
procedureAndFunctionDeclarationPart :: ParsingData -> ParsingData
procedureAndFunctionDeclarationPart parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = procedureAndFunctionDeclarationPart ( procedureDeclaration parsingData)
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = procedureAndFunctionDeclarationPart ( functionDeclaration parsingData)
    | otherwise
        = parsingData

-- ProcedureDeclaration ⟶ ProcedureHeading ";" Block ";"
procedureDeclaration :: ParsingData -> ParsingData
procedureDeclaration parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = semic_match ( block ( semic_match ( procedureHeading parsingData)))
    | otherwise
        = syntaxError parsingData

-- FunctionDeclaration ⟶ FunctionHeading ";" Block ";"
functionDeclaration :: ParsingData -> ParsingData
functionDeclaration parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = semic_match ( block ( semic_match ( functionHeading parsingData)))
    | otherwise
        = syntaxError parsingData

-- ProcedureHeading ⟶ "procedure" procedureIdentifier OptionalFormalParameterList
procedureHeading :: ParsingData -> ParsingData
procedureHeading parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_PROCEDURE"
        = optionalFormalParameterList ( procedureIdentifier ( match parsingData))
    | otherwise
        = syntaxError parsingData

-- FunctionHeading ⟶ "function" functionIdentifier OptionalFormalParameterList ":" Type
functionHeading :: ParsingData -> ParsingData
functionHeading parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FUNCTION"
        = typeParser ( colon_match ( optionalFormalParameterList ( functionIdentifier ( match parsingData))))
    | otherwise
        = syntaxError parsingData

-- OptionalFormalParameterList ⟶ "(" FormalParameterSection FormalParameterSectionTail ")"
--                             ⟶ ε
optionalFormalParameterList :: ParsingData -> ParsingData
optionalFormalParameterList parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = r_paren_match ( formalParameterSectionTail ( formalParameterSection ( match parsingData)))
    | otherwise
        = parsingData

-- FormalParameterSectionTail ⟶ ";" FormalParameterSection FormalParameterSectionTail
--                            ⟶ ε
formalParameterSectionTail :: ParsingData -> ParsingData
formalParameterSectionTail parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_SCOLON"
        = formalParameterSectionTail ( formalParameterSection ( match parsingData))
    | otherwise
        = parsingData

-- FormalParameterSection ⟶ ValueParameterSection
--                        ⟶ VariableParameterSection
formalParameterSection :: ParsingData -> ParsingData
formalParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = valueParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = variableParameterSection
    | otherwise
        = syntaxError parsingData

-- ValueParameterSection ⟶ IdentifierList ":" Type
valueParameterSection :: ParsingData -> ParsingData
valueParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IDENTIFIER"
        = typeParser ( colon_match ( identifierList parsingData))
    | otherwise
        = syntaxError parsingData

-- VariableParameterSection ⟶ "var" IdentifierList ":" Type
variableParameterSection :: ParsingData -> ParsingData
variableParameterSection parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_VAR"
        = typeParser ( colon_match ( identifierList ( match parsingData)))
    | otherwise
        = syntaxError parsingData

-- StatementPart ⟶ CompoundStatement
statementPart :: ParsingData -> ParsingData
statementPart parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_BEGIN"
        = compoundStatement parsingData
    | otherwise
        = syntaxError parsingData

-- CompoundStatement ⟶ "begin" StatementSequence "end"
compoundStatement :: ParsingData -> ParsingData
compoundStatement parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_BEGIN"
        = end_match ( statementSequence ( match parsingData))
    | otherwise
        = syntaxError parsingData

-- StatementSequence ⟶ Statement StatementTail
statementSequence :: ParsingData -> ParsingData
statementSequence parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_BEGIN", "MP_READ", "MP_WRITE", "MP_IDENTIFIER", "MP_IF", "MP_WHILE", "MP_REPEAT", "MP_FOR"]
        = statementTail ( statement ( parsingData))
    | otherwise
        = syntaxError parsingData

-- StatementTail ⟶ ";" Statement StatementTail
--               ⟶ ε
statementTail :: ParsingData -> ParsingData
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
        = syntaxError parsingData

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
        = syntaxError parsingData

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
        = syntaxError parsingData

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
        = syntaxError parsingData

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
        = syntaxError parsingData

-- Factor                  ⟶ UnsignedInteger
--                         ⟶ VariableIdentifier
--                         ⟶ "not" Factor
--                         ⟶ "(" Expression ")"
--                         ⟶ FunctionIdentifier OptionalActualParameterList
factor :: ParsingData -> ParsingData
factor parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = variableIdentifier parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = optionalActualParemeterList (functionIdentifier parsingData)
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_INTEGER_LIT", "MP_FIXED_LIT", "MP_FLOAT_LIT"]
        = match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_NOT"
        = factor (match parsingData)
    | unwrapToken (lookAheadToken parsingData) == "MP_LPAREN"
        = r_paren_match (expression (match parsingData))
    | otherwise
        = syntaxError parsingData

-- ProgramIdentifier       ⟶ Identifier
programIdentifier :: ParsingData -> ParsingData
programIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = match parsingData
    | otherwise
        = syntaxError parsingData

-- VariableIdentifier      ⟶ Identifier
variableIdentifier :: ParsingData -> ParsingData
variableIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = match parsingData
    | otherwise
        = syntaxError parsingData

-- ProcedureIdentifier     ⟶ Identifier
procedureIdentifier :: ParsingData -> ParsingData
procedureIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = match parsingData
    | otherwise
        = syntaxError parsingData

-- FunctionIdentifier      ⟶ Identifier
functionIdentifier :: ParsingData -> ParsingData
functionIdentifier parsingData
    | hasFailed parsingData == True
        = parsingData
    | getTokenType (lookAheadToken parsingData) ==  "ReservedWord"
        = match parsingData
    | otherwise
        = syntaxError parsingData

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
        = syntaxError parsingData

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
        = syntaxError parsingData

-- IdentifierList          ⟶ Identifier IdentifierTail
identifierList :: ParsingData -> ParsingData
identifierList parsingData
    | hasFailed parsingData == True
        = parsingData
    | any (unwrapToken (lookAheadToken parsingData) ==) ["MP_IDENTIFIER", "MP_STRING_LIT"]
        = identifierList (match parsingData)
    | otherwise
        = syntaxError parsingData

-- IdentifierTail          ⟶ "," Identifier IdentifierTail
--                         ⟶ ε
identifierTail :: ParsingData -> ParsingData
identifierTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = identifierTail (identifier (match parsingData))
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
        = r_paren_match (readParameterTail (readParameter (l_paren_match (match parsingData))))
    | otherwise
        = syntaxError parsingData

--ReadParameterTail ⟶ "," ReadParameter ReadParameterTail
--                  ⟶ ε
readParameterTail :: ParsingData -> ParsingData
readParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = readParameterTail (readParameter (match parsingData))
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
        = r_paren_match (writeParameterTail (writeParameter (l_paren_match (match parsingData))))
    | otherwise
        = syntaxError parsingData

--WriteParameterTail  ⟶ "," WriteParameter
--                    ⟶ ε
writeParameterTail :: ParsingData -> ParsingData
writeParameterTail parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_COMMA"
        = writeParameter (match parsingData)
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
        = expression (assignment_match (variableIdentifier parsingData))
    | getTokenType (lookAheadToken parsingData) == "ReservedWord"
        = expression (assignment_match (functionIdentifier parsingData))  --Just for now, need to get clarification from Rocky
    | otherwise
        = syntaxError parsingData

--IfStatement ⟶ "if" BooleanExpression "then" Statement OptionalElsePart
ifStatement :: ParsingData -> ParsingData
ifStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_IF"
        = optionalElsePart (statment (then_match (booleanExpression (match parsingData))))
    | otherwise
        = syntaxError parsingData

--OptionalElsePart ⟶ "else" Statement
--                 ⟶ ε
optionalElsePart :: ParsingData -> ParsingData
optionalElsePart parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_ELSE"
        = statement (match parsingData)
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
        = booleanExpression (until_match (statementSequence (match parsingData)))
    | otherwise
        = syntaxError parsingData

--WhileStatement ⟶ "while" BooleanExpression "do" Statement
whileStatement :: ParsingData -> ParsingData
whileStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_WHILE"
        = statement (do_match (booleanExpression (match parsingData)))
    | otherwise
        = syntaxError parsingData

--ForStatement ⟶ "for" ControlVariable ":=" InitialValue StepValue FinalValue "do" Statement
forStatement :: ParsingData -> ParsingData
forStatement parsingData
    | hasFailed parsingData == True
        = parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_FOR"
        = statement (do_match (finalValue (stepValue (initialValue (assignment_match (controlVariable (match parsingData)))))))
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
        = match parsingData
    | unwrapToken (lookAheadToken parsingData) == "MP_DOWNTO"
        = match parsingData
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
        = r_paren_match (actualParameterTail (actualParameter (match parsingData)))
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
        = actualParameterTail (actualParameter (match parsingData))
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
