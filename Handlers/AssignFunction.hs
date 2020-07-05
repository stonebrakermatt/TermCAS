{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 -
 - Main file -}
module AssignFunction where


handle_assign_function :: [Char] -> [E.Expression] -> E.Expression -> Int -> C.Context -> IO ()
handle_assign_function f args expr n context
    | length (filter (\arg -> case arg of E.Id x -> True ; _ -> False) args) == length args =
        case U.create_context_entry (E.FCall f args) expr of
            Just entry -> do
                putStrLn ("Successfully created function " ++ C.show_entry entry)
                repl n (context `U.context_insert` entry)
            _ -> do
                putStrLn "Error occurred while creating function"
                repl n context
    | otherwise = do
        putStrLn "Error: cannot bind to this expression"
        repl n context
