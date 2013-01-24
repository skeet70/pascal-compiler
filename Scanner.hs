module Scanner
(
    getToken,
    skipWhitespace
) where

-- | Starts with the input string (assuming at first character of next lexeme),
-- an empty lexeme, and the current line and column numbers.
-- | Returns all the same things, expecting the reciever to extract the final
-- token
import Data.Char (isDigit, isLetter)

getToken :: (String, String, Int, Int) -> (String, String, Int, Int)
getToken (source, lexeme, columnNumber, lineNumber)
    | nextChar == ' '
        = skipWhitespace (source, lexeme, newCol = columnNumber + 1, lineNumber)
    | nextChar == '('
        = lParenFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | nextChar == ')'
        = rParenFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | nextChar == ';'
        = semicolonFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | nextChar == ':'
        = colonFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | isLetter nextChar
        = letterFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    | isDigit nextChar
        = digitFSA (nextChar : source, lexeme, columnNumber, lineNumber)
    where nextChar = head source  -- get the next character


-- | skipWhitespace expects to recieve parameters that have already skipped the
-- whitespace, and it just calls getToken again.
skipWhitespace :: (String, String, Int, Int) -> (String, String, Int, Int)
skipWhitespace (source, lexeme, columnNumber, lineNumber) =
    getToken (source, lexeme, columnNumber, lineNumber)
