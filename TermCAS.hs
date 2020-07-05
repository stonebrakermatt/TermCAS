{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 -
 - Main file -}
module Main where
import System.IO
import qualified IO.Command as D
import qualified IO.Parser as P
import qualified ExpData.Context.Type as C
import qualified ExpData.Expression.Type as E
import qualified ExpData.Context.Utils as U

{- Calls the main loop with a turn number of zero
 - and empty context -}
main = repl 0 []

{- Helper for spacing the prompt -}
prompt_spaces :: Int -> [Char]
prompt_spaces n = if n > 999 
    then " "
    else take (4 - length (show n)) (repeat ' ') 

{- Main program loop -}
repl :: Int -> C.Context -> IO ()
repl n context = do
    putStr (show n ++ prompt_spaces n ++ "=> ")
    hFlush stdout
    str <- getLine
    case P.parse_input str of
        Left cmd -> case cmd of
            D.Builtin b -> handle_builtin b n context
            D.Assign e1 e2 -> handle_assign e1 e2 n context
            _ -> do
                putStrLn ("out: " ++ show cmd)
                repl (n + 1) context
        Right err -> do
            putStrLn ("Error occurred while parsing input: " ++ show err)
            repl n context

handle_assign_function :: [Char] -> [E.Expression] -> E.Expression -> Int -> C.Context -> IO ()
handle_assign_function f args expr n context = repl n context
handle_assign_variable x expr n context = repl n context

handle_assign :: E.Expression -> E.Expression -> Int -> C.Context -> IO ()
handle_assign e1 e2 n context = case e1 of
    E.FCall f args -> handle_assign_function f args e2 n context
    E.Id x -> handle_assign_variable x e2 n context
    _ -> putStrLn "Error: cannot bind to this expression"

{- Handler for builtin commands such as \about, \bindings,
 - \exit, and \help -}
handle_builtin :: D.Builtin -> Int -> C.Context -> IO ()
handle_builtin (D.About) n context = do
    sequence_ about_dialog
    repl n context
handle_builtin (D.Bindings) n context = do
    putStrLn ""
    putStrLn "Current bindings:"
    sequence_ (map (putStrLn . show) context)
    putStrLn ""
    repl n context
handle_builtin (D.Exit) n context = return ()
handle_builtin (D.Help) n context = do
    sequence_ help_dialog
    repl n context

{- About, help, and welcome messages -}
about_dialog :: [IO ()]
about_dialog =
    [ putStrLn ""
    , putStrLn "TermCAS is free software. Do whatever you want with it."
    , putStrLn "" ]
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
welcome_dialog :: [IO ()]
welcome_dialog =
    [ putStrLn ""
    , putStrLn "TermCAS v.0.1.0 - try \"\\about\" or \"\\help\""
    , putStrLn "\"\\exit\" to quit"
    , putStrLn "" ]



{- Creates a fake, temporary, context for function arguments so
 - as to prevent a dependency error on the rvalue expression -}
argcontext args =
    let eliminate (Just a) = a
        eliminate (Nothing) = ((E.Id "pi", E.Num "pi"))
    in
        let maybe_context = map (\x -> U.create_context_entry x x) args
        in map eliminate maybe_context
