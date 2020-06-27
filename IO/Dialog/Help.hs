{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for printing the help message
 - program -}
module IO.Dialog.Help where





{- Help -}
help :: IO ()
help = sequence_ help_dialog

help_dialog :: [IO ()] 
help_dialog = 
    [ putStrLn ""
    , putStrLn "Basic usage:"
    , putStrLn ""
    , putStrLn "1) \"<var>\""
    , putStrLn "In this case, the expression will be evaluated."
    , putStrLn ""
    , putStrLn "2) \"<var>=<expr>\"" 
    , putStrLn "In this case, a variable bound to the string \"var\""
    , putStrLn "will be created (or overwritten, if one existed)."
    , putStrLn "These variables can then be used in evaluating expressions."
    , putStrLn "" ]
