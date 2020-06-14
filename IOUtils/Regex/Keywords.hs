{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 -
 - File defining the keywords for 
 - special string literals recognized
 - by this program -}
module IOUtils.Regex.Keywords 
    ( special_funcs
    , discrete_ops 
    , constants ) where





trig_funcs =
    [ "sin"
    , "cos"
    , "tan"
    , "arcsin"
    , "arccos"
    , "arctan" ]

hyp_trig_funcs = 
    [ "sinh"
    , "cosh"
    , "tanh"
    , "arcsinh"
    , "arccosh"
    , "arctanh" ]

special_funcs = ["exp", "log", "ln"]
    ++ trig_funcs
    ++ hyp_trig_funcs

discrete_ops = 
    [ "mod"
    , "choose"
    , "permute" ]

constants = 
    [ "pi"
    , "tau"
    , "e"
    , "i"
    , "phi"] 
