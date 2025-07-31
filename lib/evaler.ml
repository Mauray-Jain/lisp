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
    | Fixnum(v) -> (Fixnum(v), env)
    | Boolean(v) -> (Boolean(v), env)
    | Symbol(v) -> (Symbol(v), env)
    | Nil -> (Nil, env)
    | Pair(Symbol "if", Pair(cond, Pair(iftrue, Pair(iffalse, Nil)))) ->
        eval_sexp (eval_if cond iftrue iffalse) env
        (*
            if we want to keep local variables of if in local scope
            let (result, _) = eval_sexp ...
            (result, env)
            Notice we discard the env of local scope
        *)
    | _ -> (sexp, env)
