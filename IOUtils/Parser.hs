{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for parsing user input -}
module IOUtils.Parser where
import qualified IOUtils.Command as D
import qualified IOUtils.Lexer as L
import qualified IOUtils.Tokens as T

import qualified Expression as E

type Parser a = [T.InputToken] -> a


split_equals :: Parser (Maybe ([T.InputToken], [T.InputToken]))
split_equals tokens = 
    let split_equals' [] revtokens = Nothing
        split_equals' (tok : tokens) revtokens = case tok of
            T.DelimiterToken "=" -> Just (tokens, reverse revtokens)
            _ -> split_equals' tokens (tok : revtokens)
    in split_equals' tokens []

lex_input :: L.Lexer Char
lex_input = L.remove_spaces . L.lex


-- parse_command :: Parser D.Command

