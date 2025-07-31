open Types

exception TypeError of string

let basis =
    let prim_plus = function
        | [Fixnum(a); Fixnum(b)] -> Fixnum(a+b)
        | _ -> raise (TypeError "(+ int int)")
    in
    let prim_pair = function
        | [a; b] -> Pair(a, b)
        | _ -> raise (TypeError "(pair a b)")
    in
    let new_prim acc (name, func) =
        Env.bind (name, Primitive(name, func), acc)
    in
    List.fold_left new_prim Nil [
        ("+", prim_plus);
        ("pair", prim_pair)
    ]

let rec eval_sexp sexp env =
    let eval_if cond iftrue iffalse =
        let (condval, _) = eval_sexp cond env in
        match condval with
        | Boolean(true) -> iftrue
        | Boolean(false) -> iffalse
        | _ -> raise (TypeError "(if bool e1 e2)")
    in
    match sexp with
    | Fixnum(v) -> Fixnum(v), env
    | Boolean(v) -> Boolean(v), env
    | Symbol(v) -> Env.lookup (v, env), env
    | Nil -> Nil, env
    | Primitive(n, f) -> Primitive(n, f), env
    | Pair(_, _) when is_list sexp ->
        (
            match pair_to_list sexp with
            | [Symbol "if"; cond; iftrue; iffalse] ->
                fst (eval_sexp (eval_if cond iftrue iffalse) env), env
            | [Symbol "env"] -> env, env
            | [Symbol "val"; Symbol name; value] ->
                let (result, _) = eval_sexp value env in
                let env' = Env.bind (name, result, env) in
                result, env'
            | (Symbol fn)::args ->
                let func = eval_sexp (Symbol fn) env in
                (*Evaluate the arguments*)
                let eval_arg e = fst (eval_sexp e env) in
                let argvals = List.map eval_arg args in
                (
                    match func with
                    | Primitive(_, f), _ -> (f argvals, env)
                    | _ -> raise (TypeError "(apply func args)")
                )
            | _ -> sexp, env
        )
    | _ -> sexp, env
