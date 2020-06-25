{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling execution context
 - program -}
module Context where
import qualified Data.Expression as E
import qualified IOUtils.Command as D





{- Types for storing set variables -}
data ContextEntry
    = Function ([Char], [E.Expression], E.Expression)
    | Variable ([Char], E.Expression)
    deriving Read
type Context = [ContextEntry]

{- For showing a context entry -}
instance Show ContextEntry where
    show (Function (f, args, expr)) = show (E.FCall f args) ++ " = " ++ show expr
    show (Variable (x, expr)) = show (E.Id x) ++ " = " ++ show expr

{- Utility to create a context entry -}
create_context_entry :: E.Expression -> E.Expression -> Maybe ContextEntry
create_context_entry e1 e2 = case e1 of
    E.Id var -> Just (Variable (var, e2))
    E.FCall func args -> Just (Function (func, args, e2))
    _ -> Nothing

{- Gets the string a context entry is bound to -}
get_string_binding :: ContextEntry -> [Char]
get_string_binding (Function (f, args, expr)) = f
get_string_binding (Variable (v, exprs)) = v

{- Takes existing context and adds a new entry,
 - overwriting an entry if one exists that is bound
 - to the same string -}
context_insert :: Context -> ContextEntry -> Context
context_insert context entry = 
    let context_insert' [] entry revcontext = reverse (entry : revcontext)
        context_insert' (c : context) entry revcontext
            | get_string_binding c /= get_string_binding entry =
                context_insert' context entry (c : revcontext)
            | otherwise = reverse revcontext ++ (entry : context)
    in context_insert' context entry []
            
