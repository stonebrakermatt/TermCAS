{- TermCAS
 - v0.1.0
 - (c) 2020 Matt Stonebraker
 - 
 - File for parsing user input -}
module IOUtils.Parser where
import qualified IOUtils.Command as D
import qualified IOUtils.Lexer as L
import qualified IOUtils.Token as T
import qualified Expression as E
import qualified Operator as O





{- Grammar Rules: 
 -
 - Expr -> AAtail
 - 
 - Atail -> XAAtail | Empty
 - A -> BBtail
 - Btail -> YBBtail | Empty
 - B -> CCtail
 - Ctail -> ZCCtail | Empty
 - C -> DDtail
 - Dtail -> \^DDtail | Empty
 - 
 - D -> \-D | F
 - F -> IdOrCall | Num | Paren
 -
 - X -> Mod | Choose | Permute
 - Y -> \+ | \-
 - Z -> \* | \
 -
 - Paren -> \( Expr \)
 - IdOrCall -> Id (Args | Empty)
 - Args -> \( Expr Argtail \)
 - Argtail -> \, Expr Argtail | Empty -}



{- Bind utility for chaining our
 - mutually recursive parsing functions -}
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just val) f = f val

{- Opposite of bind, basically -}
fails :: Maybe a -> (() -> Maybe a) -> Maybe a
fails Nothing f = f()
fails ok _ = ok



{- Parser type for simplpifying type signatures -}
type Parser t = [T.InputToken] -> Maybe (t, [T.InputToken])



{- Utilities for the simplifying the parser as
 - much as possible. Still complicated, but it helps -}

{- Gets the next token.
 - Plays nice with bind and fails -}
next_token :: Parser T.InputToken
next_token (t : input1) = Just (t, input1)
next_token _ = Nothing

{- Extracts a parenthetical for nested parsing.
 - Used for function call arguments as well as
 - simple parenthetical expressions -}
extract_parenthetical :: Parser [T.InputToken]
extract_parenthetical input =
    let extract_parenthetical' depth [] revtokens = Nothing
        extract_parenthetical' depth (t : tokens) revtokens = case t of
            T.DelimiterToken "(" -> 
                extract_parenthetical' (depth + 1) tokens (t : revtokens)
            T.DelimiterToken ")" -> 
                if depth == 1
                then Just (reverse revtokens, tokens)
                else extract_parenthetical' (depth - 1) tokens (t : revtokens)
            _ -> extract_parenthetical' depth tokens (t : revtokens)
    in extract_parenthetical' 1 input []

{- Separates function call arguments into a list
 - of expressions. It is built to be mindful of 
 - nesting depth to avoid splitting the arguments
 - if one of the function call arguments itself
 - contains a multivariable function call -}
separate_args :: Parser [E.Expression]
separate_args input =
    let separate_args' depth (t : tokens) revtokens revargs = case t of
            T.DelimiterToken "(" -> 
                separate_args' (depth + 1) tokens (t : revtokens) revargs
            T.DelimiterToken ")" -> 
                if depth == 0 
                then Nothing
                else separate_args' (depth - 1) tokens (t : revtokens) revargs
            T.DelimiterToken "," -> 
                if depth == 0
                then case parse_expr (reverse revtokens) of
                    Just (new_arg, _) -> separate_args' depth tokens [] (new_arg : revargs)
                    Nothing -> Nothing
                else separate_args' depth tokens (t : revtokens) revargs
            _ -> separate_args' depth tokens (t : revtokens) revargs
        separate_args' depth [] revtokens revargs = if length revtokens > 0
            then case parse_expr (reverse revtokens) of
                Just (new_arg, _) -> Just (reverse (new_arg : revargs), [])
                Nothing -> Nothing
            else Just (reverse revargs, []) 
    in separate_args' 0 input [] []

{- Parses an operator with the appropriate precedence 
 - Used in parse_atail through parse_dtail to maintain
 - the order of operaions -}
parse_op_prec :: Int -> Parser O.Op
parse_op_prec n [] = Nothing
parse_op_prec n (t : input) = case t of
    T.OpToken "!" -> Nothing
    T.OpToken o -> 
        if O.operator_precedence (O.get_op o) == n
        then Just (O.get_op o, input)
        else Nothing
    _ -> Nothing

{- Helper function for parse_a through parse_d. Takes 
 - the results of parse_atail through parse_dtail and 
 - builds out the expression tree -}
build_tail :: ([E.Expression], [O.Op]) -> Maybe E.Expression
build_tail ([], _) = Nothing
build_tail ([e], _) = Just e
build_tail (_, []) = Nothing
build_tail (e1 : e2 : exps, o : ops) = 
    build_tail (E.Binary o e1 e2 : exps, ops)



{- The main parser functionality starts below -}

{- Rule: Expr -> AAtail -}
parse_expr :: Parser E.Expression
parse_expr input =
    parse_a input `bind` (\(e, input1) ->
    parse_atail e input1 `bind` (\(pairs, input2) ->
    build_tail pairs `bind` (\exp ->
    Just (exp, input2))))

{- Rule: A -> BBtail -}
parse_a :: Parser E.Expression
parse_a input =
    parse_b input `bind` (\(e, input1) ->
    parse_btail e input1 `bind` (\(pairs, input2) ->
    build_tail pairs `bind` (\exp ->
    Just (exp, input2))))

{- Rule: Atail -> XAAtail | Empty -}
parse_atail :: E.Expression -> Parser ([E.Expression], [O.Op])
parse_atail exp input = 
    let parse_atail' [] (exps, ops) = Just ((reverse exps, reverse ops), [])
        parse_atail' input (exps, ops) =
            parse_op_prec 1 input `bind` (\(o, input1) ->
            parse_a input1 `bind` (\(e, input2) ->
            parse_atail' input2 (e : exps, o : ops))) `fails` (\() ->
            Just ((reverse exps, reverse ops), input))
    in parse_atail' input ([exp], [])

{- Rule: B -> CCtail -}
parse_b :: Parser E.Expression
parse_b input =
    parse_c input `bind` (\(e, input1) ->
    parse_ctail e input1 `bind` (\(pairs, input2) ->
    build_tail pairs `bind` (\exp ->
    Just (exp, input2))))

{- Rule: Btail -> YBBtail | Empty -}
parse_btail :: E.Expression -> Parser ([E.Expression], [O.Op])
parse_btail exp input = 
    let parse_btail' [] (exps, ops) = Just ((reverse exps, reverse ops), [])
        parse_btail' input (exps, ops) =
            parse_op_prec 2 input `bind` (\(o, input1) ->
            parse_b input1 `bind` (\(e, input2) ->
            parse_btail' input2 (e : exps, o : ops))) `fails` (\() ->
            Just ((reverse exps, reverse ops), input))
    in parse_btail' input ([exp], [])

{- Rule: C -> DDtail -}
parse_c :: Parser E.Expression
parse_c input =
    parse_d input `bind` (\(e, input1) ->
    parse_dtail e input1 `bind` (\(pairs, input2) ->
    build_tail pairs `bind` (\exp ->
    Just (exp, input2))))

{- Rule: Ctail -> ZCCtail | Empty -}
parse_ctail :: E.Expression -> Parser ([E.Expression], [O.Op])
parse_ctail exp input = 
    let parse_ctail' [] (exps, ops) = Just ((reverse exps, reverse ops), [])
        parse_ctail' input (exps, ops) =
            parse_op_prec 3 input `bind` (\(o, input1) ->
            parse_c input1 `bind` (\(e, input2) ->
            parse_ctail' input2 (e : exps, o : ops))) `fails` (\() ->
            Just ((reverse exps, reverse ops), input))
    in parse_ctail' input ([exp], [])

{- Rule: D -> \-D | F -}
parse_d :: Parser E.Expression
parse_d input =
    next_token input `bind` (\(t, input1) ->
    case t of
        T.OpToken "-" -> 
            parse_d input1 `bind` (\(e, input2) ->
            Just (E.Negate e, input2))
        _ -> parse_f input)

{- Rule: Dtail -> \^DDtail | Empty -}
parse_dtail :: E.Expression -> Parser ([E.Expression], [O.Op])
parse_dtail exp input = 
    let parse_dtail' [] (exps, ops) = Just ((reverse exps, reverse ops), [])
        parse_dtail' input (exps, ops) =
            parse_op_prec 4 input `bind` (\(o, input1) ->
            parse_d input1 `bind` (\(e, input2) ->
            parse_dtail' input2 (e : exps, o : ops))) `fails` (\() ->
            Just ((reverse exps, reverse ops), input))
    in parse_dtail' input ([exp], [])
    
{- Rule: F -> IdOrCall | Num | Paren -}
parse_f :: Parser E.Expression
parse_f input = 
    parse_id_or_call input `fails` (\() ->
    parse_num input `fails` (\() ->
    parse_parens input))

{- Helper functions for parse_id_or_call -}
parse_id :: Parser E.Expression
parse_id input =
    next_token input `bind` (\(t, input1) ->
    case t of 
        T.IdToken id -> Just (E.Id id, input1)
        _ -> Nothing)
parse_call :: Parser [E.Expression]
parse_call input = 
    next_token input `bind` (\(t, input1) ->
    case t of 
        T.DelimiterToken "(" -> 
            extract_parenthetical input1 `bind` (\(parens, input2) ->
            separate_args parens `bind` (\(args, _) -> 
            Just (args, input2)))
        _ -> Nothing)
    
{- Rule: IdOrCall -> Id (Args | Empty) -}
parse_id_or_call :: Parser E.Expression
parse_id_or_call input =
    parse_id input `bind` (\(eid, input1) ->
    parse_call input1 `bind` (\(args, input2) -> 
    case eid of
        E.Id id -> Just (E.FCall id args, input2)
        _ -> Nothing)) `fails` (\() -> parse_id input)

{- Parses a number or parenthetical if
 - parse_id_or_call fails -}
parse_num :: Parser E.Expression 
parse_num input = 
    next_token input `bind` (\(t, input1) ->
    case t of
        T.NumLiteralToken num -> Just (E.Num num, input1)
        _ -> Nothing)
parse_parens :: Parser E.Expression
parse_parens input = 
    next_token input `bind` (\(t, input1) ->
    case t of 
        T.DelimiterToken "(" ->
            extract_parenthetical input1 `bind` (\(parens, input2) ->
            parse_f parens `bind` (\(exp, _) -> -- parse_expr
            Just (E.Parenthetical exp, input2)))
        _ -> Nothing)
