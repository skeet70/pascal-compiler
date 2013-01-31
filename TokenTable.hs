module TokenTable where
--Authored by: Tyler J. Huffman
data ReservedWords = MP_AND | MP_BEGIN | MP_DIV | MP_DO | MP_DOWNTO | MP_ELSE
    | MP_END | MP_FIXED | MP_FLOAT | MP_FOR | MP_FUNCTION | MP_IF
    | MP_INTEGER | MP_MOD | MP_NOT | MP_OR | MP_PROCEDURE | MP_PROGRAM
    | MP_READ | MP_REPEAT | MP_THEN | MP_TO | MP_UNTIL | MP_VAR
    | MP_WHILE | MP_WRITE
    deriving (Eq, Show, Read)

data IdentifiersAndLiterals = MP_IDENTIFIER | MP_INTEGER_LIT | MP_FIXED_LIT
    | MP_FLOAT_LIT | MP_STRING_LIT
    deriving (Eq, Show, Read)

data ErrorCodes = MP_RUN_COMMENT | MP_RUN_STRING | MP_ERROR
    deriving (Eq, Show, Read)

data Symbols = MP_PERIOD | MP_COMMA | MP_SCOLON | MP_LPAREN | MP_EQUAL
    | MP_GTHAN | MP_LTHAN | MP_LEQUAL | MP_NEQUAL | MP_ASSIGN | MP_PLUS
    | MP_MINUS | MP_TIMES | MP_COLON | MP_GEQUAL
    deriving (Eq, Show, Read)

data EndOfFile = MP_EOF
    deriving (Eq, Show, Read)

data Token = ReservedWords ReservedWords | IdentifiersAndLiterals IdentifiersAndLiterals | ErrorCodes ErrorCodes 
    | Symbols Symbols | EndOfFile EndOfFile
    deriving (Eq, Show, Read)

unwrapToken :: Token -> String
unwrapToken (ReservedWords x)           = show x
unwrapToken (IdentifiersAndLiterals x)  = show x
unwrapToken (ErrorCodes x)              = show x
unwrapToken (Symbols x)                 = show x
unwrapToken (EndOfFile x)               = show x

getTokenType :: Token -> String
getTokenType (ReservedWords x)          = "ReservedWords"
getTokenType (IdentifiersAndLiterals x) = "IdentifiersAndLiterals"
getTokenType (ErrorCodes x)             = "ErrorCodes"
getTokenType (Symbols x)                = "Symbols"
getTokenType (EndOfFile x)              = "EndOfFile"
