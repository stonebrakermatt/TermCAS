{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - Main file -}
module Main where
import System.IO
import qualified IO.Command as D
import qualified IO.Dialog.About as A
import qualified IO.Dialog.Help as H
import qualified IO.Dialog.Welcome as W
import qualified IO.Parser as P
import qualified ExpData.ContextUtils as C
import qualified ExpData.Expression as E 





{- Main function -}
main :: IO ()
main = repl 0 []

{- Stores a counter and a context of 
 - variable bindings and recurses -}
repl :: Int -> E.Context -> IO ()
repl n context = if n == 0
    then do 
        W.welcome
        repl 1 context
    else do
        putStr $ show n ++ " => "
        hFlush stdout
        str <- getLine
        case P.parse_input str of 
            Just D.Exit -> return ()
            Just D.About -> do
                A.about
                repl n context
            Just D.Bindings -> do
                putStrLn ""
                putStrLn "Current bindings:"
                sequence_ (map (putStrLn . show) context)
                putStrLn ""
                repl n context
            Just D.Help -> do
                H.help
                repl n context
            Just (D.Assign e1 e2) -> handle_assign n context e1 e2 
            Just (D.Eval e) -> handle_eval n context e         
            _ -> do
                putStrLn "Error occurred while parsing input"
                repl n context



{- Creates a fake, temporary, context for function arguments so
 - as to prevent a dependency error on the rvalue expression -}
argcontext args = 
    let eliminate (Just a) = a
        eliminate (Nothing) = E.Variable ("pi", E.Num "pi")
    in 
        let maybe_context = map (\x -> C.create_context_entry x x) args
        in map eliminate maybe_context

{- Makes sure functions are defined in terms of a 
 - simple variable expression -}
filterfunction arg = case arg of
    E.Id x -> True
    _ -> False

{- Handler for assigning a variable or function a definition -}
handle_assign :: Int -> E.Context -> E.Expression -> E.Expression -> IO ()
handle_assign n context e1 e2 = case e1 of
    E.FCall f args -> 
        if length (filter filterfunction args) == length args
            then
                if (argcontext args ++ context) 
                    `E.satisfies_dependencies` 
                    (E.get_dependencies e2)
                    then case C.create_context_entry e1 e2 of
                        Just entry -> repl (n + 1) (C.context_insert context entry)
                        Nothing -> do
                            putStrLn "Error occurred while creating variable"
                            repl n context
                    else do
                        putStrLn "Dependencies not satisfied for this expression"
                        repl n context
            else do
                putStrLn "Error with function arguments"
                repl n context
    E.Id x -> 
        if context `E.satisfies_dependencies` (E.get_dependencies e2)
            then case C.create_context_entry e1 e2 of
                Just entry -> repl (n + 1) (C.context_insert context entry)
                Nothing -> do
                    putStrLn "Error occurred while creating variable"
                    repl n context
            else do
                putStrLn "Dependencies not satisfied for this expression"
                repl n context
    _ -> do
        putStrLn "Cannot bind to this expression"
        repl n context
   
{- Handler for evaluating an expression -}
handle_eval :: Int -> E.Context ->  E.Expression -> IO ()
handle_eval n context e = if context `E.satisfies_dependencies` (E.get_dependencies e)
    then do
        putStrLn ("OUT: " ++ (show (e `E.apply_all_context` context)))
        repl (n + 1) context
    else do
        sequence_ (map (putStrLn . show) (E.get_dependencies e))
        putStrLn "Dependencies not satisfied for this expression"
        repl n context
