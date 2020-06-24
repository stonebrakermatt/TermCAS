{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - Main file -}
module Main where
import System.IO
import qualified Context as C
import qualified IOUtils.Command as D
import qualified Dialog.Help as H
import qualified IOUtils.Lexer as L
import qualified IOUtils.Parser as P





main :: IO ()
main = repl

repl :: IO ()
repl = do
    putStr "=> "
    hFlush stdout
    str <- getLine
    if str /= "\\exit"
        then
            if str == "\\help"
            then do
                H.help
                main
            else do
                (putStrLn . show) (P.bind (P.parse_input str) C.create_context_entry)
                main
        else return ()
