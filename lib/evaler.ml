open Types

let basis =
    let prim_plus = function
        | [Fixnum a; Fixnum b] -> Fixnum (a + b)
        | _ -> raise (TypeError "(+ int int)")
    in
    let prim_pair = function
        | [a; b] -> Pair (a, b)
        | _ -> raise (TypeError "(pair a b)")
    in
    let rec prim_list = function
        | [] -> Nil
        | a::b -> Pair (a, prim_list b)
    in
    let new_prim acc (name, func) =
        Env.bind (name, Primitive (name, func), acc)
    in
    List.fold_left new_prim [] [
        ("+", prim_plus);
        ("pair", prim_pair);
        ("list", prim_list)
    ]

let rec evalexp exp env =
    let evalapply f vs =
        match f with
        | Primitive (_, f) -> f vs
        | Closure (ns, e, clenv) -> evalexp e (Env.bindlist ns vs clenv)
        | _ -> raise (TypeError "(apply prim '(args)) or (prim args)")
    in
    let rec ev = function
        | Literal Quote q -> q
        | Literal l -> l
        | Var n -> Env.lookup (n, env)
        | If (c, t, _) when ev c = Boolean true -> ev t
        | If (c, _, f) when ev c = Boolean false -> ev f
        | If _ -> raise (TypeError "(if bool e1 e2)")
        | And (c1, c2) ->
            begin
                match (ev c1, ev c2) with
                | (Boolean v1, Boolean v2) -> Boolean (v1 && v2)
                | _ -> raise (TypeError "(and bool bool)")
            end
        | Or (c1, c2) ->
            begin
                match (ev c1, ev c2) with
                | (Boolean v1, Boolean v2) -> Boolean (v1 || v2)
                | _ -> raise (TypeError "(or bool bool)")
            end
        | Apply (fn, e) -> evalapply (ev fn) (pair_to_list (ev e))
        | Call (Var "env", []) -> env_to_val env
        | Call (fn, args) -> evalapply (ev fn) (List.map ev args)
        | Lambda (ns, body) -> Closure (ns, body, env)
        | Defexp _ -> raise ThisCan'tHappenError
    in
    ev exp

let evaldef defexp env =
    match defexp with
    | Val (n, e) ->
        let v = evalexp e env in
        (v, Env.bind (n, v, env))
    | Def (n, ns, e) ->
        (* let (formals, body, cl_env) = ( *)
        (*     match evalexp (Lambda (ns, e)) env with *)
        (*     | Closure (fs, bod, env) -> (fs, bod, env) *)
        (*     | _ -> raise (TypeError "Expecting closure.") *)
        (* ) in *)
        let (formals, body, cl_env) = (ns, e, env) in
        let loc = Env.mkloc () in
        let closure = Closure (formals, body, Env.bindloc (n, loc, cl_env)) in
        let () = loc := Some closure in
        (closure, Env.bindloc (n, loc, env))
    | Exp e -> evalexp e env, env

let eval ast env =
    match ast with
    | Defexp d -> evaldef d env
    | e -> evalexp e env, env
