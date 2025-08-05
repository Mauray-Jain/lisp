open Types

let rec build_ast sexp =
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
            | [Symbol "and"; c1; c2] ->
                And (build_ast c1, build_ast c2)
            | [Symbol "or"; c1; c2] ->
                Or (build_ast c1, build_ast c2)
            | [Symbol "val"; Symbol n; e] ->
                Defexp (Val (n, build_ast e))
            | [Symbol "lambda"; ns; e] when is_list ns ->
                let err () = raise (TypeError "(lambda (formals) body)") in
                let args = List.map (function | Symbol s -> s | _ -> err ()) (pair_to_list ns) in
                Lambda (args, build_ast e)
            | [Symbol "define"; Symbol n; ns; e] ->
                let err () = raise (TypeError "(define name (formals) body)") in
                let args = List.map (function | Symbol s -> s | _ -> err ()) (pair_to_list ns) in
                Defexp (Def (n, args, build_ast e))
            | [Symbol "apply"; fnexp; args] ->
                Apply (build_ast fnexp, build_ast args)
            | fnexp::args -> Call (build_ast fnexp, List.map build_ast args)
            | [] -> raise (ParseError "Poorly formed expression")
        )
    | Pair _ -> Literal sexp
    | Closure _ -> raise ThisCan'tHappenError

let rec string_exp =
    let spacesep es = String.concat " " (List.map string_exp es) in
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
        "(" ^ string_exp fn ^ spacesep args ^ ")"
    | Defexp (Val (n, e)) ->
        "(val " ^ n ^ " " ^ string_exp e ^ ")"
    | Defexp (Def (n, ns, e)) ->
        "(define " ^ n ^ "(" ^ String.concat " " ns ^ ")" ^ string_exp e ^ ")"
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
