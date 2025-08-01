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
    let rec prim_list = function
        | [] -> Nil
        | a::b -> Pair(a, prim_list b)
    in
    let new_prim acc (name, func) =
        Env.bind (name, Primitive(name, func), acc)
    in
    List.fold_left new_prim Nil [
        ("+", prim_plus);
        ("pair", prim_pair);
        ("list", prim_list)
    ]

let evalexp exp env =
    let evalapply f es =
        match f with
        | Primitive(_, f) -> f es
        | _ -> raise (TypeError "(apply prim '(args)) or (prim args)")
    in
    let rec ev = function
        | Literal l -> l
        | Var n -> Env.lookup (n, env)
        | If(c, t, _) when ev c = Boolean true -> ev t
        | If(c, _, f) when ev c = Boolean false -> ev f
        | If _ -> raise (TypeError "(if bool e1 e2)")
        | And(c1, c2) ->
            begin
                match (ev c1, ev c2) with
                | (Boolean v1, Boolean v2) -> Boolean (v1 && v2)
                | _ -> raise (TypeError "(and bool bool)")
            end
        | Or(c1, c2) ->
            begin
                match (ev c1, ev c2) with
                | (Boolean v1, Boolean v2) -> Boolean (v1 || v2)
                | _ -> raise (TypeError "(or bool bool)")
            end
        | Apply(fn, e) -> evalapply (ev fn) (pair_to_list (ev e))
        | Call(Var "env", []) -> env
        | Call(fn, args) -> evalapply (ev fn) (List.map ev args)
        | Defexp _ -> raise ThisCan'tHappenError
    in
    ev exp

let evaldef defexp env =
    match defexp with
    | Val(name, exp) ->
        let v = evalexp exp env in
        (v, Env.bind (name, v, env))
    | Exp e -> evalexp e env, env

let eval ast env =
    match ast with
    | Defexp d -> evaldef d env
    | e -> evalexp e env, env
