{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module Data.Expression where
import qualified IOUtils.Capture as C
import qualified IOUtils.Regex.GrammarRegexes as G
import qualified IOUtils.Regex.Keywords as K
import qualified Data.Operator as O





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



{- Classifying expressions for more thoughtful output -}
is_rational :: Expression -> Bool
is_rational (Negate e) = is_rational e
is_rational (Binary o e1 e2)
    | is_rational e1 && is_rational e2 = case o of
        O.Power -> is_integer e2
        O.Choose -> is_integer e1 && is_integer e2
        O.Permute -> is_integer e1 && is_integer e2
        _ -> True
    | otherwise = False
is_rational (Parenthetical e) = is_rational e
is_rational (FCall f args) = False -- Come back to this
is_rational (Id i) = False -- And this
is_rational (Num n)
    | (n == "pi") || (n == "e") || (n == "phi") = False
    | otherwise = True

is_integer :: Expression -> Bool
is_integer (Negate e) = is_integer e
is_integer (Binary o e1 e2)
    | is_integer e1 && is_integer e2 = case o of
        O.DivBy -> False
        _ -> True
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



{- Types for storing set variables -}
data ContextEntry
    = Function ([Char], [Expression], Expression)
    | Variable ([Char], Expression)
    deriving Read
type Context = [ContextEntry]

{- For showing a context entry -}
instance Show ContextEntry where
    show (Function (f, args, expr)) = show (FCall f args) ++ " = " ++ show expr
    show (Variable (x, expr)) = show (Id x) ++ " = " ++ show expr



{- Types for expression dependencies.
 - Function and number of arguments or simply variable -}
data DependencyKind 
    = F Int 
    | V
    deriving (Show, Read)
type ExpressionDependency = (DependencyKind, [Char])



{- Utility for getting the dependencies of the expression -}
get_dependencies :: Expression -> [ExpressionDependency]
get_dependencies (Negate e) = get_dependencies e
get_dependencies (Binary o e1 e2) = 
    (get_dependencies e1) ++ (get_dependencies e2)
get_dependencies (Parenthetical e) = get_dependencies e
get_dependencies (FCall f args) = 
    (F (length args), f) : foldr (++) [] (map get_dependencies args)
get_dependencies (Id x) = [(V, x)]
get_dependencies (Num n) = []
