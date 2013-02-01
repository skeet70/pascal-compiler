--Authored by: Tyler J. Huffman
module Scanner.TokenTable where

data ReservedWord = MP_AND | MP_BEGIN | MP_DIV | MP_DO | MP_DOWNTO | MP_ELSE
    | MP_END | MP_FIXED | MP_FLOAT | MP_FOR | MP_FUNCTION | MP_IF
    | MP_INTEGER | MP_MOD | MP_NOT | MP_OR | MP_PROCEDURE | MP_PROGRAM
    | MP_READ | MP_REPEAT | MP_THEN | MP_TO | MP_UNTIL | MP_VAR
    | MP_WHILE | MP_WRITE
    deriving (Eq, Show, Read)

data IdentifierOrLiteral = MP_IDENTIFIER | MP_INTEGER_LIT | MP_FIXED_LIT
    | MP_FLOAT_LIT | MP_STRING_LIT
    deriving (Eq, Show, Read)

data ErrorCode = MP_RUN_COMMENT | MP_RUN_STRING | MP_ERROR
    deriving (Eq, Show, Read)

data Symbol = MP_PERIOD | MP_COMMA | MP_SCOLON | MP_LPAREN | MP_RPAREN
    | MP_EQUAL | MP_GTHAN | MP_LTHAN | MP_LEQUAL | MP_NEQUAL | MP_ASSIGN
    | MP_PLUS | MP_MINUS | MP_TIMES | MP_COLON | MP_GEQUAL
    deriving (Eq, Show, Read)

data EndOfFile = MP_EOF
    deriving (Eq, Show, Read)

data Token = ReservedWord ReservedWord | IdentifierOrLiteral IdentifierOrLiteral
    | ErrorCode ErrorCode | Symbol Symbol | EndOfFile EndOfFile
    deriving (Eq, Show, Read)

unwrapToken :: Token -> String
unwrapToken (ReservedWord x)         = show x
unwrapToken (IdentifierOrLiteral x)  = show x
unwrapToken (ErrorCode x)            = show x
unwrapToken (Symbol x)               = show x
unwrapToken (EndOfFile x)            = show x

getTokenType :: Token -> String
getTokenType (ReservedWord x)        = "ReservedWord"
getTokenType (IdentifierOrLiteral x) = "IdentifierOrLiteral"
getTokenType (ErrorCode x)           = "ErrorCode"
getTokenType (Symbol x)              = "Symbol"
getTokenType (EndOfFile x)           = "EndOfFile"
