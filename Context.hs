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

create_context_entry :: D.Command -> Maybe ContextEntry
create_context_entry command = case command of
    D.Assign e1 e2 -> case e1 of
        E.Id var -> Just (Variable (var, e2))
        E.FCall func args -> Just (Function (func, args, e2))
        _ -> Nothing
    _ -> Nothing
