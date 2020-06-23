{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - Main file -}
module Main where
import System.IO
import qualified IOUtils.Command as D
import qualified IOUtils.Lexer as L
import qualified IOUtils.Parser as P





main :: IO ()
main = do
    putStr "=> "
    hFlush stdout
    str <- getLine
    if str == "\\exit"
    then return ()
    else if str == "\\help"
    then return ()
    else do
        (putStrLn . show) (P.parse_input str)
        main
