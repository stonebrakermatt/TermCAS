{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for lexing user input -}
module IOUtils.Lexer where

import qualified IOUtils.Capture as C
import qualified IOUtils.Regex.GrammarRegexes as G
import qualified IOUtils.Regex.Type as R
import qualified IOUtils.Tokens as T


type Lexer a = [a] -> [T.InputToken]
type MatchContext a = [(R.Regex a, [a] -> T.InputToken)]
type MatchResult a = ([a], T.InputToken)

match_context :: MatchContext Char
match_context = zip G.language_regexes T.token_constructors

match :: [Char] -> MatchResult Char
match user_input =
    let match' user_input [] = (user_input, T.SpaceToken "")
        match' user_input ((re, tokenizer) : match_context) =
            case user_input `C.capture` re of
                Nothing -> match' user_input match_context
                Just (tok, input_tail) -> (input_tail, tokenizer tok)
    in match' user_input match_context

lex :: Lexer Char
lex user_input = 
    let lex' [] revtokens = revtokens
        lex' uinput revtokens = 
            let (input_tail, tok) = match uinput
            in lex' input_tail (tok : revtokens)
    in lex' user_input []

remove_spaces :: Lexer T.InputToken
remove_spaces revtokens = 
    let remove_spaces' [] tokens = tokens
        remove_spaces' (tok : revtokens) tokens = case tok of
            T.SpaceToken _ -> remove_spaces' revtokens tokens
            _ -> remove_spaces' revtokens (tok : tokens)
    in remove_spaces' revtokens []
