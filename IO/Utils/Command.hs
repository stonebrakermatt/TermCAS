{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module IO.Utils.Command where
import qualified ExpData.Expression as E





data Command 
    = Eval E.Expression
    | Assign E.Expression E.Expression
    | About
    | Bindings
    | Exit
    | Help
    deriving (Read, Show)
