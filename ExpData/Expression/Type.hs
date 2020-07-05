{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 -
 - File for handling commands
 - program -}
module ExpData.Expression.Type where
import qualified ExpData.Expression.Operator as O
import qualified IO.Utils.Regex.Keywords as K




data Expression
    = Negate Expression
    | Binary O.Op Expression Expression
    | Parenthetical Expression
    | FCall [Char] [Expression]
    | Id [Char]
    | Num [Char]
    deriving (Read, Eq)



{- Helpers for showing args -}
get_arg_str :: [Expression] -> [String]
get_arg_str exprs = map show exprs
add_commas :: [String] -> String -> String
add_commas [] revStr = reverse revStr
add_commas (s : []) revStr = reverse (s ++ revStr)
add_commas (s1 : s2 : strs) revStr = add_commas (s2 : strs) ("," ++ s1 ++ revStr)

{- How to print expressions -}
instance Show Expression where
    show (Negate e) = case e of
        Negate e' -> show e'
        Binary o e1 e2 -> "-(" ++ show e ++ ")"
        _ -> "-" ++ show e
    show (Binary o e1 e2) = show e1 ++ show o ++ show e2
    show (Parenthetical e) = "(" ++ show e ++ ")"
    show (FCall f args) = f ++ "(" ++ (add_commas (get_arg_str args) []) ++ ")"
    show (Id i) = i
    show (Num n) = n
