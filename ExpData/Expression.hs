{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module ExpData.Expression where
import qualified IO.Utils.Capture as C
import qualified IO.Utils.Regex.GrammarRegexes as G
import qualified IO.Utils.Regex.Keywords as K
import qualified ExpData.Operator as O





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
     


{- Utility for substituting an expression for a variable -}
substitute :: [Char] -> Expression -> Expression -> Expression
substitute x e expr = case expr of
    Negate e1 -> Negate (substitute x e e1)
    Binary o e1 e2 -> Binary o (substitute x e e1) (substitute x e e2)
    Parenthetical e1 -> Parenthetical (substitute x e e1)
    Id y -> if y == x 
        then e
        else Id y
    FCall f args -> FCall f (map (\e1 -> substitute x e e1) args)
    Num n -> Num n

substitute_args :: [Expression] -> [Expression] -> Expression -> Expression
substitute_args [] [] expr = expr
substitute_args (a1 : argvars) (a2 : arglist) expr = case a1 of
    Id x -> substitute_args argvars arglist (substitute x a2 expr)
    _ -> expr



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
    deriving (Show, Read, Eq)
type ExpressionDependency = (DependencyKind, [Char])



{- Utility for mapping context entries to dependencies -}
context_to_dependency :: ContextEntry -> ExpressionDependency
context_to_dependency (Function (f, args, expr)) = (F (length args), f)
context_to_dependency (Variable (v, expr)) = (V, v)

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

{- Utility for seeing if a given context satisfies a dependency list -}
satisfies_dependencies :: Context -> [ExpressionDependency] -> Bool
satisfies_dependencies _ [] = True
satisfies_dependencies [] _ = False
satisfies_dependencies context (d : deps) = case get_dependency context d of
    Just _ -> satisfies_dependencies context deps
    Nothing -> False

{- Looks up a context entry for a specific dependency -}
get_dependency :: Context -> ExpressionDependency -> Maybe ContextEntry
get_dependency [] _ = Nothing
get_dependency (c : context) dep
    | context_to_dependency c == dep = Just c
    | otherwise = get_dependency context dep

{- Apply context entry to an expression -}
apply_context :: Expression -> ContextEntry -> Expression
apply_context e (Function (f, args, expr)) = case e of
    Negate e1 -> Negate (apply_context e1 (Function (f, args, expr)))
    Binary o e1 e2 -> 
        Binary o 
            (apply_context e1 (Function (f, args, expr)))
            (apply_context e2 (Function (f, args, expr)))
    Parenthetical e1 -> Parenthetical (apply_context e1 (Function (f, args, expr)))
    FCall f1 args1 -> if f1 == f
        then substitute_args args args1 expr
        else FCall f1 args1
    other -> other
apply_context e (Variable (x, expr)) = substitute x expr e

{- Applies many context entries -}
apply_all_context :: Expression -> Context -> Expression
apply_all_context expr [] = expr
apply_all_context expr (c : context) = 
    apply_all_context (apply_context expr c) context
