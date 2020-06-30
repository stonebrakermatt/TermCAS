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



{- Types for expression dependencies.
 - Function and number of arguments or simply variable -}
data DependencyKind 
    = F Int 
    | V
    deriving (Read, Eq)
type ExpressionDependency = (DependencyKind, [Char])

instance Show DependencyKind where
    show (F n) = "Function of " ++ (show n) ++ " arguments"
    show (V) = "Variable"


{- Utility for getting the dependencies of the expression -}
get_dependencies :: Expression -> [ExpressionDependency]
get_dependencies (Negate e) = get_dependencies e
get_dependencies (Binary o e1 e2) = 
    (get_dependencies e1) ++ (get_dependencies e2)
get_dependencies (Parenthetical e) = get_dependencies e
get_dependencies (FCall f args)
    | (length args == 1) && (f `elem` K.special_funcs) = get_dependencies (head args)
    | otherwise = (F (length args), f) : foldr (++) [] (map get_dependencies args)
get_dependencies (Id x) = [(V, x)]
get_dependencies (Num n) = []
