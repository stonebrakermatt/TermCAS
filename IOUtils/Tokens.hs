{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 -
 - File for defining valid user input
 - tokens to match while lexing -}
module IOUtils.Tokens where





data InputToken 
    = IdToken [Char]
    | OpToken [Char]
    | NumLiteralToken [Char]
    | DelimiterToken [Char]
    | SpaceToken [Char]
    | BadInputToken [Char]
    deriving (Read, Eq)

token_constructors = 
    [ DelimiterToken
    , OpToken
    , NumLiteralToken
    , IdToken
    , SpaceToken
    , BadInputToken ]

instance Show InputToken where
    show (IdToken str) = "<id:" ++ str ++ ">"
    show (OpToken str) = "<op:" ++ str ++ ">"
    show (NumLiteralToken str) = "<num:" ++ str ++ ">" 
    show (DelimiterToken str) = "<del:" ++ str ++ ">"
    show (SpaceToken str) = "<sp:" ++ str ++ ">" 
    show (BadInputToken str) = "<err:" ++ str ++ ">"
