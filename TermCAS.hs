{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - Main file -}
module Main where
import System.IO
import qualified Dialog.About as A
import qualified Data.ContextUtils as C
import qualified IOUtils.Command as D
import qualified Data.Expression as E 
import qualified Dialog.Help as H
import qualified IOUtils.Lexer as L
import qualified IOUtils.Parser as P
import qualified Dialog.Welcome as W





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
        case str of 
            "\\exit" -> return ()
            "\\about" -> do
                A.about
                repl n context
            "\\bindings" -> do
                putStrLn ""
                putStrLn "Current bindings:"
                sequence_ (map (putStrLn . show) context)
                putStrLn ""
                repl n context
            "\\help" -> do
                H.help
                repl n context
            _ -> 
                let parsed_input = P.parse_input str
                in case parsed_input of
                    Just (D.Assign e1 e2) -> case C.create_context_entry e1 e2 of
                        Just entry -> do 
                            repl (n + 1) (C.context_insert context entry)
                        Nothing -> do
                            putStrLn "Error occurred while creating variable"
                            repl (n + 1) context
                    Just (D.Eval e) -> do
                        (putStrLn . show) e
                        sequence_ (map (putStrLn . show) (E.get_dependencies e))
                        repl (n + 1) context
                    _ -> do
                        putStrLn "Error occurred while parsing input"
                        repl (n + 1) context
