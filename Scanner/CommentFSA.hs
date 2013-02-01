import Scanner.TokenTable
import Scanner.Dispatcher

--Author Tyler J. Huffman
--Top-level state machine to remove comments from a source string
--Returns the ErrorCodes MP_RUN_STRING token if no FSA is valid
commentFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
commentFSA (src, lexeme, column_number, line_number)
    | stringHead == Just '{'
        = bracketFSA (tail src, lexeme, column_number, line_number)
    | stringHead == Just '(' && stringNext == Just '*'
        = parenFSA (tail (tail src), lexeme, column_number, line_number)
    | otherwise
        = (src, lexeme, ErrorCodes MP_ERROR, column_number, line_number)
    where stringHead = if src == [] then Nothing else Just (head src)
          stringNext
              | stringHead /= Nothing && tail src /= [] = Just (src !! 1)
              | otherwise = Nothing

--Low-level state machine to run when a { comment is found
--Returns the IdentifiersAndLiterals MP_STRING_LIT token
bracketFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
bracketFSA (src, lexeme, column_number, line_number)
    | stringHead == Just '}'
        = getToken (tail src, lexeme , column_number, line_number)
    | stringHead == Nothing
        = (src, lexeme, ErrorCodes MP_RUN_STRING, column_number, line_number)
    | otherwise
        = bracketFSA(tail src, lexeme, column_number, line_number)
    where stringHead = if src == [] then Nothing else Just (head src)

--Low-level state machine to run when a (* comment is found
--Returns the IdentifiersAndLiterals MP_STRING_LIT token
parenFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
parenFSA (src, lexeme, column_number, line_number)
    | stringHead == Just '*' && stringNext == Just ')'
        = getToken (tail (tail src), lexeme, column_number, line_number)
    | stringHead == Nothing
        = (src, lexeme, ErrorCodes MP_RUN_STRING, column_number, line_number)
    | otherwise
        = parenFSA (tail src, lexeme, column_number, line_number)
    where stringHead = if src == [] then Nothing else Just (head src)
          stringNext
              | stringHead /= Nothing && tail src /= [] = Just (src !! 1)
              | otherwise = Nothing
