{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 -
 - File for handling commands
 - program -}
module ExpData.Context.Type where
import qualified ExpData.Expression.Type as E





type ContextEntry = (E.Expression, E.Expression)
type Context = [ContextEntry]

show_entry :: ContextEntry -> [Char]
show_entry entry = show (fst entry) ++ " = " ++ show (snd entry)
