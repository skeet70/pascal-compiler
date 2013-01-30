import TokenTable
--Author Tyler J. Huffman
--Top-level state machine to remove comments from a source string
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

bracketFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
bracketFSA (src, lexeme, column_number, line_number)
    | stringHead == Just '}'
        = (tail src, lexeme, IdentifiersAndLiterals MP_STRING_LIT, column_number, line_number)
    | stringHead == Nothing
        = (src, lexeme, ErrorCodes MP_RUN_STRING, column_number, line_number)
    | otherwise
        = bracketFSA(tail src, lexeme, column_number, line_number)
    where stringHead = if src == [] then Nothing else Just (head src)

parenFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
parenFSA (src, lexeme, column_number, line_number)
    | stringHead == Just '*' && stringNext == Just ')'
        = (tail (tail src), lexeme, IdentifiersAndLiterals MP_STRING_LIT, column_number, line_number)
    | stringHead == Nothing
        = (src, lexeme, ErrorCodes MP_RUN_STRING, column_number, line_number)
    | otherwise
        = parenFSA (tail src, lexeme, column_number, line_number)
    where stringHead = if src == [] then Nothing else Just (head src)
          stringNext
              | stringHead /= Nothing && tail src /= [] = Just (src !! 1)
              | otherwise = Nothing
