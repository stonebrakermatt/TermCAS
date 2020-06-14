{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module Expression where




data Op
    = Power
    | Times
    | DivBy
    | Plus
    | Minus
    | Mod
    | Choose
    | Permute
    deriving (Show, Read, Eq)



data Expression
    = Negate Expression
    | Factorial Expression
    | Binary Op Expression Expression
    | Parenthetical Expression
    | FCall Expression [Expression]
    | Id [Char]
    | Num [Char]
    deriving (Show, Read, Eq)
