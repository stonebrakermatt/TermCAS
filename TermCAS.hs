{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - Main file -}
module Main where
import System.IO
import qualified IO.Dialog.About as A
import qualified ExpData.ContextUtils as C
import qualified IO.Utils.Command as D
import qualified ExpData.Expression as E 
import qualified IO.Dialog.Help as H
import qualified IO.Utils.Parser as P
import qualified IO.Dialog.Welcome as W





main :: IO ()
main = repl 0 []

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
            Just (D.Assign e1 e2) -> case C.create_context_entry e1 e2 of
                Just entry -> do 
                    repl (n + 1) (C.context_insert context entry)
                Nothing -> do
                    putStrLn "Error occurred while creating variable"
                    repl n context
            Just (D.Eval e) -> do
                (putStrLn . show) e
                sequence_ (map (putStrLn . show) (E.get_dependencies e))
                repl (n + 1) context
            _ -> do
                putStrLn "Error occurred while parsing input"
                repl n context
