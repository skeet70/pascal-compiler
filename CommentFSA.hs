--Author Tyler J. Huffman
--Top-level state machine to remove comments from a source string
commentFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
commentFSA (src, lexeme, column_number, line_number) 
    | stringHead == '{'
        = bracketFSA (tail src, lexeme, column_number + 1, line_number)
    | stringHead == '(' && stringNext == '*'
        = parenFSA (tail . tail src, lexeme, column_number + 2, line_number)
    | otherwise
        = (src, lexeme, ErrorCodes MP_ERROR, column_number, line_number)
    where stringHead = if src == [] then ' ' else head src
          stringNext
              | stringHead /= ' ' && tail src /= [] = src !! 1
              | otherwise = 'x'

bracketFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
bracketFSA (src, lexeme, column_number, line_number)
    | stringHead == '}'
        = (tail src, lexeme, IdentifiersAndLiterals MP_STRING_LIT, column_number + 1, line_number)
    | stringHead == ' '
        = (src, lexeme, ErrorCodes MP_RUN_STRING, column_number, line_number)
    | otherwise
        = bracketFSA(tail src, lexeme, column_number + 1, line_number)
    where stringHead = if src == [] then ' ' else head src

parenFSA :: (String, String, Int, Int) -> (String, String, Token, Int, Int)
parenFSA (src, lexeme, column_number, line_number)
    | stringHead == '*' && stringNext == ')'
        = (tail . tail src, lexeme, IdentifiersAndLiterals MP_STRING_LIT, column_number + 2, line_number)
    | stringHead == ' '
        = (src, lexeme, ErrorCodes MP_RUN_STRING, column_number, line_number)
    | otherwise
        = parenFSA (tail src, lexeme, column_number + 1, line_number)
    where strigHead = if src == [] then ' ' else head src
          stringNext
              | stringHead /= ' ' && tail src /= [] = src !! 1
              | otherwise = 'x'
