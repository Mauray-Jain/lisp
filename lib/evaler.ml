open Types

exception TypeError of string

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
    | Nil -> (Nil, env)
    | Pair(_, _) when is_list sexp ->
        (
            match pair_to_list sexp with
            | [Symbol "if"; cond; iftrue; iffalse] ->
                (*
                    if we want to keep local variables of if in local scope
                    let (result, _) = eval_sexp ...
                    (result, env)
                    Notice we discard the env of local scope
                *)
                eval_sexp (eval_if cond iftrue iffalse) env
            | [Symbol "env"] -> env, env
            | [Symbol "pair"; car; cdr] ->
                Pair(car, cdr), env
            | [Symbol "val"; Symbol name; value] ->
                let (result, _) = eval_sexp value env in
                let env' = Env.bind (name, result, env) in
                result, env'
            | _ -> sexp, env
        )
    | _ -> sexp, env
