{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - Main file -}
module Main where
import System.IO
import qualified Dialog.About as A
import qualified Context as C
import qualified IOUtils.Command as D
import qualified Dialog.Help as H
import qualified IOUtils.Lexer as L
import qualified IOUtils.Parser as P
import qualified Dialog.Welcome as W





main :: IO ()
main = repl 0

repl :: Int -> IO ()
repl n = if n == 0
    then do 
        W.welcome
        repl 1
    else do
        putStr "=> "
        hFlush stdout
        str <- getLine
        if str /= "\\exit"
            then
                if str == "\\help"
                then do
                    H.help
                    repl (n + 1)
                else if str == "\\about" 
                    then do
                        A.about
                        repl (n + 1)
                    else do 
                        (putStrLn . show) (P.bind (P.parse_input str) C.create_context_entry)
                        repl (n + 1)
            else return ()
