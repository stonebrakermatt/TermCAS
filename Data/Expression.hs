{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module Data.Expression where
import qualified IOUtils.Capture as C
import qualified IOUtils.Regex.GrammarRegexes as G
import qualified Data.Operator as O





data Expression
    = Negate Expression
    | Binary O.Op Expression Expression
    | Parenthetical Expression
    | FCall [Char] [Expression]
    | Id [Char]
    | Num [Char]
    deriving (Read, Eq)
    


{- Classifying expressions for more thoughtful output -}
is_integer :: Expression -> Bool
is_integer (Negate e) = is_integer e
is_integer (Binary o e1 e2)
    | is_integer e1 && is_integer e2 = True
    | otherwise = False
is_integer (Parenthetical e) = is_integer e
is_integer (FCall f args) = False -- Come back to this
is_integer (Id i) = False -- And this
is_integer (Num n) = case n `C.capture` G.regex_int of
    Nothing -> False
    Just (match, rem) -> 
        if rem == []
            then True
            else False

{- Helpers for showing args -}
get_arg_str :: [Expression] -> [String]
get_arg_str exprs = map show exprs
add_commas :: [String] -> String -> String
add_commas [] revStr = reverse revStr
add_commas (s : []) revStr = reverse (s ++ revStr)
add_commas (s1 : s2 : strs) revStr = add_commas (s2 : strs) ("," ++ s1 ++ revStr)        

{- How to print expressions -}
instance Show Expression where
    show (Negate e) = "-(" ++ show e ++ ")"
    show (Binary o e1 e2) = show e1 ++ show o ++ show e2
    show (Parenthetical e) = "(" ++ show e ++ ")"
    show (FCall f args) = f ++ "(" ++ (add_commas (get_arg_str args) []) ++ ")"
    show (Id i) = i
    show (Num n) = n
