{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for printing the about message
 - program -}
module IO.Dialog.About where





{- Help -}
about :: IO ()
about = sequence_ about_dialog

about_dialog :: [IO ()] 
about_dialog = 
    [ putStrLn ""
    , putStrLn "TermCAS is free software. Do whatever you want with it."
    , putStrLn "" ]
