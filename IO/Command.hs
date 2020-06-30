{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module IO.Command where
import qualified ExpData.Expression.Type as E





data Builtin
    = About
    | Bindings
    | Exit
    | Help
    deriving (Read, Show, Eq)

data Command 
    = Eval E.Expression
    | Assign E.Expression E.Expression
    | Builtin Builtin
    deriving (Read, Show, Eq)
