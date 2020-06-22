{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module Expression where
import qualified Operator as O





data Expression
    = Negate Expression
    | Factorial Expression
    | Binary O.Op Expression Expression
    | Parenthetical Expression
    | FCall [Char] [Expression]
    | Id [Char]
    | Num [Char]
    deriving (Show, Read, Eq)
