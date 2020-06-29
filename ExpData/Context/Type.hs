{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module ExpData.Context.Type where
import qualified ExpData.Expression.Type as E





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
