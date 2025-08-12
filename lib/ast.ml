open Types

exception UniqueError of string

let rec assert_unique = function
    | [] -> ()
    | x::xs ->
        if List.mem x xs then raise (UniqueError x) else assert_unique xs

let rec build_ast sexp =
    let rec cond_to_ifs = function
        | [] -> Literal (Symbol "error")
        | Pair (Symbol "else", Pair (res, Nil))::[] ->
            If (build_ast (Boolean true), build_ast res, cond_to_ifs [])
        | Pair (cond, Pair (res, Nil))::condpairs ->
            If (build_ast cond, build_ast res, cond_to_ifs condpairs)
        | _ -> raise (TypeError "(cond conditions)")
    in
    let let_kinds = ["let", LET; "let*", LETSTAR; "letrec", LETREC] in
    let valid_let s = List.mem_assoc s let_kinds in
    let to_kind s = List.assoc s let_kinds in
    match sexp with
    | Primitive _ -> raise ThisCan'tHappenError
    | Nil | Fixnum _ | Boolean _ | Quote _ -> Literal sexp
    | Symbol s -> Var s
    | Pair _ when is_list sexp ->
        (
            match pair_to_list sexp with
            | [Symbol "quote"; sexp] -> Literal (Quote sexp)
            | [Symbol "if"; cond; iftrue; iffalse] ->
                If (build_ast cond, build_ast iftrue, build_ast iffalse)
            | (Symbol "cond")::conditions -> cond_to_ifs conditions
            | [Symbol "and"; c1; c2] ->
                And (build_ast c1, build_ast c2)
            | [Symbol "or"; c1; c2] ->
                Or (build_ast c1, build_ast c2)
            | [Symbol "val"; Symbol n; e] ->
                Defexp (Val (n, build_ast e))
            | [Symbol "lambda"; ns; e] when is_list ns ->
                let err () = raise (TypeError "(lambda (formals) body)") in
                let args = List.map (function | Symbol s -> s | _ -> err ()) (pair_to_list ns) in
                let () = assert_unique args in
                Lambda (args, build_ast e)
            | [Symbol "define"; Symbol n; ns; e] ->
                let err () = raise (TypeError "(define name (formals) body)") in
                let args = List.map (function | Symbol s -> s | _ -> err ()) (pair_to_list ns) in
                let () = assert_unique args in
                Defexp (Def (n, args, build_ast e))
            | [Symbol "apply"; fnexp; args] ->
                Apply (build_ast fnexp, build_ast args)
            | (Symbol s)::bindings::exp::[] when valid_let s && is_list bindings ->
                let mkbindings = function
                    | Pair (Symbol n, Pair (e, Nil)) -> n, build_ast e
                    | _ -> raise (TypeError "(let bindings exp)")
                in
                let bindings = pair_to_list bindings |> List.map mkbindings in
                let () = assert_unique (List.map fst bindings) in
                Let (to_kind s, bindings, build_ast exp)
            | fnexp::args -> Call (build_ast fnexp, List.map build_ast args)
            | [] -> raise (ParseError "Poorly formed expression")
        )
    | Pair _ -> Literal sexp
    | Closure _ -> raise ThisCan'tHappenError

let rec string_exp =
    let spacesep e = String.concat " " e in
    let spacesep_exp es = spacesep (List.map string_exp es) in
    let string_of_binding (n, v) = "(" ^ n ^ " " ^ string_exp v ^ ")" in
    function
    | Literal l -> string_val l
    | Var v -> v
    | If (c, t, f) ->
        "(if " ^ string_exp c ^ " " ^ string_exp t ^ " " ^ string_exp f ^ ")"
    | And (c1, c2) ->
        "(and " ^ string_exp c1 ^ " " ^ string_exp c2 ^ ")"
    | Or (c1, c2) ->
        "(or " ^ string_exp c1 ^ " " ^ string_exp c2 ^ ")"
    | Lambda _ ->
        "#<lambda>"
    | Apply (fn, args) ->
        "(apply " ^ string_exp fn ^ " " ^ string_exp args ^ ")"
    | Call (fn, args) ->
        "(" ^ string_exp fn ^ spacesep_exp args ^ ")"
    | Let (kind, l, e) ->
        let str = match kind with
            | LET -> "let"
            | LETSTAR -> "let*"
            | LETREC -> "letrec"
        in
        let bindings = List.map string_of_binding l in
        "(" ^ str ^ " (" ^ spacesep bindings ^ ")" ^ string_exp e ^ ")"
    | Defexp (Val (n, e)) ->
        "(val " ^ n ^ " " ^ string_exp e ^ ")"
    | Defexp (Def (n, ns, e)) ->
        "(define " ^ n ^ "(" ^ spacesep ns ^ ")" ^ string_exp e ^ ")"
    | Defexp (Exp e) -> string_exp e

and string_val e =
    let rec string_list l =
        match l with
        | Pair (a, Nil) -> string_val a
        | Pair (a, b) -> string_val a ^ " " ^ string_list b
        | _ -> raise ThisCan'tHappenError
    in
    let string_pair p =
        match p with
        | Pair (a, b) -> string_val a ^ " . " ^ string_val b
        | _ -> raise ThisCan'tHappenError
    in
    match e with
    | Fixnum num -> string_of_int num
    | Boolean b -> if b then "#t" else "#f"
    | Symbol v -> v
    | Nil -> "nil"
    | Pair _ when is_list e -> "(" ^ string_list e ^ ")"
    | Pair _ -> "(" ^ string_pair e ^ ")"
    | Primitive (name, _) -> "#<primitive:" ^ name ^">"
    | Quote q -> "'" ^ string_val q
    | Closure _ -> "#<closure>"
