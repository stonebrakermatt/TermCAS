{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 -
 - File for handling commands
 - program -}
module ExpData.Context.Utils where
import qualified ExpData.Context.Type as C
import qualified ExpData.Expression.Type as E





create_context_entry :: E.Expression -> E.Expression -> Maybe C.ContextEntry
create_context_entry e expr = case e of
    E.Id x -> Just (e, expr)
    E.FCall f args -> Just (e, expr)
    _ -> Nothing

context_insert :: C.Context -> C.ContextEntry -> C.Context
context_insert [] entry = [entry]
context_insert (c : context) entry =
    let context_insert' [] entry revcontext = reverse (entry : revcontext)
        context_insert' (c : context) entry revcontext
            | fst c == fst entry = reverse revcontext ++ (entry : context)
            | otherwise = context_insert' context entry (c : revcontext)
    in context_insert' context entry []
