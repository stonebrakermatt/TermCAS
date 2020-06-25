{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling execution context
 - program -}
module Data.ContextUtils where
import qualified Data.Expression as E





{- Utility to create a context entry -}
create_context_entry :: E.Expression -> E.Expression -> Maybe E.ContextEntry
create_context_entry e1 e2 = case e1 of
    E.Id var -> Just (E.Variable (var, e2))
    E.FCall func args -> Just (E.Function (func, args, e2))
    _ -> Nothing

{- Gets the string a context entry is bound to -}
get_string_binding :: E.ContextEntry -> [Char]
get_string_binding (E.Function (f, args, expr)) = f
get_string_binding (E.Variable (v, exprs)) = v

{- Takes existing context and adds a new entry,
 - overwriting an entry if one exists that is bound
 - to the same string -}
context_insert :: E.Context -> E.ContextEntry -> E.Context
context_insert context entry = 
    let context_insert' [] entry revcontext = reverse (entry : revcontext)
        context_insert' (c : context) entry revcontext
            | get_string_binding c /= get_string_binding entry =
                context_insert' context entry (c : revcontext)
            | otherwise = reverse revcontext ++ (entry : context)
    in context_insert' context entry []
            
