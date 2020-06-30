{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling execution context
 - program -}
module ExpData.Context.Utils where
import qualified ExpData.Context.Type as C
import qualified ExpData.Expression.Type as E
import qualified ExpData.Expression.Utils as U





{- Utility to create a context entry -}
create_context_entry :: E.Expression -> E.Expression -> Maybe C.ContextEntry
create_context_entry e1 e2 = case e1 of
    E.Id var -> Just (C.Variable (var, e2))
    E.FCall func args -> Just (C.Function (func, args, e2))
    _ -> Nothing

{- Gets the string a context entry is bound to -}
get_string_binding :: C.ContextEntry -> [Char]
get_string_binding (C.Function (f, args, expr)) = f
get_string_binding (C.Variable (v, exprs)) = v

{- Takes existing context and adds a new entry,
 - overwriting an entry if one exists that is bound
 - to the same string -}
context_insert :: C.Context -> C.ContextEntry -> C.Context
context_insert context entry = 
    let context_insert' [] entry revcontext = reverse (entry : revcontext)
        context_insert' (c : context) entry revcontext
            | get_string_binding c /= get_string_binding entry =
                context_insert' context entry (c : revcontext)
            | otherwise = reverse revcontext ++ (entry : context)
    in context_insert' context entry []

{- Utility for seeing if a given context satisfies a dependency list -}
satisfies_dependencies :: C.Context -> [E.ExpressionDependency] -> Bool
satisfies_dependencies _ [] = True
satisfies_dependencies [] _ = False
satisfies_dependencies context (d : deps) = case get_dependency context d of
    Just _ -> satisfies_dependencies context deps
    Nothing -> False

{- Utility for mapping context entries to dependencies -}
context_to_dependency :: C.ContextEntry -> E.ExpressionDependency
context_to_dependency (C.Function (f, args, expr)) = (E.F (length args), f)
context_to_dependency (C.Variable (v, expr)) = (E.V, v)


{- Looks up a context entry for a specific dependency -}
get_dependency :: C.Context -> E.ExpressionDependency -> Maybe C.ContextEntry
get_dependency [] _ = Nothing
get_dependency (c : context) dep
    | context_to_dependency c == dep = Just c
    | otherwise = get_dependency context dep

{- Apply context entry to an expression -}
apply_context :: E.Expression -> C.ContextEntry -> E.Expression
apply_context e (C.Function (f, args, expr)) = case e of
    E.Negate e1 -> E.Negate (apply_context e1 (C.Function (f, args, expr)))
    E.Binary o e1 e2 -> 
        E.Binary o 
            (apply_context e1 (C.Function (f, args, expr)))
            (apply_context e2 (C.Function (f, args, expr)))
    E.Parenthetical e1 -> E.Parenthetical (apply_context e1 (C.Function (f, args, expr)))
    E.FCall f1 args1 -> if f1 == f
        then U.substitute_args args args1 expr
        else E.FCall f1 args1
    other -> other
apply_context e (C.Variable (x, expr)) = U.substitute x expr e

{- Applies many context entries -}
apply_all_context :: E.Expression -> C.Context -> E.Expression
apply_all_context expr [] = expr
apply_all_context expr (c : context) = 
    apply_all_context (apply_context expr c) context
