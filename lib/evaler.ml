open Types

let basis =
    let numprim name op =
        (name, function | [Fixnum a; Fixnum b] -> Fixnum (op a b)
                | _ -> raise (TypeError ("(" ^ name ^ " int int)")))
    in
    let cmpprim name op =
        (name, function | [Fixnum a; Fixnum b] -> Boolean (op a b)
                | _ -> raise (TypeError ("(" ^ name ^ " int int)")))
    in
    let prim_pair = function
        | [a; b] -> Pair (a, b)
        | _ -> raise (TypeError "(pair a b)")
    in
    let rec prim_list = function
        | [] -> Nil
        | a::b -> Pair (a, prim_list b)
    in
    let prim_car = function
        | [Pair (car, _)] -> car
        | _ -> raise (TypeError "(car non-nil-pair)")
    in
    let prim_cdr = function
        | [Pair (_, cdr)] -> cdr
        | _ -> raise (TypeError "(cdr non-nil-pair)")
    in
    let prim_symp = function
        | [Symbol _] -> Boolean true
        | [_] -> Boolean false
        | _ -> raise (TypeError "(sym? single-arg)")
    in
    let prim_atomp = function
        | [Pair _] -> Boolean false
        | [_] -> Boolean true
        | _ -> raise (TypeError "(atom? something)")
    in
    let prim_eqp = function
        | [a; b] -> Boolean (a = b)
        | _ -> raise (TypeError "(eq? a b)")
    in
    let prim_getchar = function
        | [] ->
            (try Fixnum (int_of_char @@ input_char stdin)
            with End_of_file -> Fixnum (-1))
        | _ -> raise (TypeError "(getchar)")
    in
    let prim_print = function
        | [v] -> let () = print_string @@ Ast.string_val v in Symbol "ok"
        | _ -> raise (TypeError "(print val)")
    in
    let prim_itoc = function
        | [Fixnum i] -> Symbol (char_of_int i |> String.make 1)
        | _ -> raise (TypeError "(itoc int)")
    in
    let prim_cat = function
        | [Symbol a; Symbol b] -> Symbol (a ^ b)
        | _ -> raise (TypeError "(cat sym sym)")
    in
    let new_prim acc (name, func) =
        Env.bind (name, Primitive (name, func), acc)
    in
    List.fold_left new_prim [] [
        numprim "+" ( + );
        numprim "-" ( - );
        numprim "*" ( * );
        numprim "/" ( / );
        cmpprim "<" ( < );
        cmpprim ">" ( > );
        cmpprim "=" ( = );
        ("pair", prim_pair);
        ("list", prim_list);
        ("car", prim_car);
        ("cdr", prim_cdr);
        ("sym?", prim_symp);
        ("atom?", prim_atomp);
        ("eq?", prim_eqp);
        ("getchar", prim_getchar);
        ("print", prim_print);
        ("itoc", prim_itoc);
        ("cat", prim_cat);
    ]

let rec evalexp exp env =
    let evalapply f vs =
        match f with
        | Primitive (_, f) -> f vs
        | Closure (ns, e, clenv) -> evalexp e (Env.bindlist ns vs clenv)
        | _ -> raise (TypeError "(apply prim '(args)) or (prim args)")
    in
    let unzip ls = (List.map fst ls, List.map snd ls) in
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
        | Let (LET, bs, body) ->
            let evbinding (n, e) = n, ref (Some (ev e)) in
            evalexp body (Env.extend (List.map evbinding bs) env)
        | Let (LETSTAR, bs, body) ->
            let evbinding acc (n, e) = Env.bind (n, evalexp e acc, acc) in
            evalexp body (List.fold_left evbinding env bs)
        | Let (LETREC, bs, body) ->
            let names, values = unzip bs in
            let env' = Env.bindloclist names (List.map Env.mkloc values) env in
            let updates = List.map (fun (n, e) -> n, Some (evalexp e env')) bs in
            let () = List.iter (fun (n, v) -> (List.assoc n env') := v) updates in
            evalexp body env'
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
