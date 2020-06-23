{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module IOUtils.Command where
import qualified Expression as E




data Command 
    = Eval E.Expression
    | Assign E.Expression E.Expression
    deriving (Read, Show)
