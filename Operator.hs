{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for handling commands
 - program -}
module Operator where





{- Op data type for all supported binary operations -}
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



{- Pairs of Op constructors and corresponding strings -}
get_op :: [Char] -> Op
get_op "mod" = Mod
get_op "choose" = Choose
get_op "permute" = Permute
get_op "+" = Plus
get_op "-" = Minus
get_op "*" = Times
get_op "/" = DivBy
get_op _ = Power

{- Defines operator precedence levels -}
operator_precedence :: Op -> Int
operator_precedence Mod = 1
operator_precedence Choose = 1
operator_precedence Permute = 1
operator_precedence Plus = 2
operator_precedence Minus = 2
operator_precedence Times = 3
operator_precedence DivBy = 3
operator_precedence Power = 4
