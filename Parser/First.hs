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


