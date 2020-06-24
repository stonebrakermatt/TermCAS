{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for printing the help message
 - program -}
module Dialog.Welcome where





{- Help -}
welcome :: IO ()
welcome = sequence_ welcome_dialog

welcome_dialog :: [IO ()] 
welcome_dialog = 
    [ putStrLn ""
    , putStrLn "TermCAS v.0.1.0 - try \"\\about\" or \"\\help\""
    , putStrLn "\"\\exit\" to quit"
    , putStrLn "" ]
