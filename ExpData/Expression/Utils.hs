{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module ExpData.Expression.Utils where
import qualified ExpData.Expression.Type as T
import qualified ExpData.Expression.Operator as O
import qualified IO.Utils.Capture as C
import qualified IO.Utils.Regex.GrammarRegexes as G





{- Utility for substituting an expression for a variable -}
substitute :: [Char] -> T.Expression -> T.Expression -> T.Expression
substitute x e expr = case expr of
    T.Negate e1 -> T.Negate (substitute x e e1)
    T.Binary o e1 e2 -> T.Binary o (substitute x e e1) (substitute x e e2)
    T.Parenthetical e1 -> T.Parenthetical (substitute x e e1)
    T.Id y -> if y == x 
        then e
        else T.Id y
    T.FCall f args -> T.FCall f (map (\e1 -> substitute x e e1) args)
    T.Num n -> T.Num n

substitute_args :: [T.Expression] -> [T.Expression] -> T.Expression -> T.Expression
substitute_args [] [] expr = expr
substitute_args (a1 : argvars) (a2 : arglist) expr = case a1 of
    T.Id x -> substitute_args argvars arglist (substitute x a2 expr)
    _ -> expr

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

simplify :: T.Expression -> Maybe Double
simplify (T.Negate e) = -(simplify e)
simplify (T.Binary o e1 e2) = case o of
simplify (T.Parenthetical e) = simplify e
simplify (T.Id y) = Nothing
simplify (T.FCall f args) = case f of
    "sin" -> Just (sin (simplify (head args)))
    "cos" -> Just (cos (simplify (head args)))
    "tan" -> Just (tan (simplify (head args)))
    "csc" -> Just (1 / sin (simplify (head args)))
    "sec" -> Just (1 / cos (simplify (head args)))
    "cot" -> Just (1 / tan (simplify (head args)))
    "arcsin" -> Just (asin (simplify (head args)))
    "arccos" -> Just (acos (simplify (head args)))
    "arctan" -> Just (atan (simplify (head args)))
    "arccsc" -> Just (asin (1 / simplify (head args)))
    "arcsec" -> Just (acos (1 / simplify (head args)))
    "arccot" -> Just (atan (1 / simplify (head args)))
    "sinh" -> Just (sinh (simplify (head args)))
    "cosh" -> Just (cosh (simplify (head args)))
    "tanh" -> Just (tanh (simplify (head args)))
    "csch" -> Just (1 / sinh (simplify (head args)))
    "sech" -> Just (1 / cosh (simplify (head args)))
    "coth" -> Just (1 / tanh (simplify (head args)))
    "arcsinh" -> Just (asinh (simplify (head args)))
    "arccosh" -> Just (acosh (simplify (head args)))
    "arctanh" -> Just (atanh (simplify (head args)))
    "arccsch" -> Just (asinh (1 / simplify (head args)))
    "arcsech" -> Just (acosh (1 / simplify (head args)))
    "arccoth" -> Just (atanh (1 / simplify (head args)))
    "exp" -> Just (simplify (T.Binary O.Power (T.Num "e") (head args)))
    "log" -> Just (log (simplify (head args))) 
    "ln" -> Just (log (simplify (head args)))
    _ -> Nothing
simplify (T.Num n) = case n of
    "pi" -> pi
    "tau" -> 2 * pi
    "e" -> 2.7182818284
    _ -> read n :: Double

