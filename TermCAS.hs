{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - Main file -}
module Main where
import System.IO
import qualified IOUtils.Lexer as L


main :: IO ()
main = do
    putStr "=> "
    hFlush stdout
    str <- getLine
    if str /= "\\exit"
        then do
            sequence_ (map (putStrLn . show) (L.remove_spaces $ L.lex str))
            main
        else return ()
