type lobject =
    | Fixnum of int
    | Boolean of bool
    | Symbol of string
    | Nil
    | Pair of lobject * lobject
    | Primitive of string * (lobject list -> lobject)
    | Quote of value
    | Closure of name list * exp * value Env.env
and value = lobject
and name = string
and exp =
    | Literal of value          (* self evaluating *)
    | Var of name               (* variable name *)
    | If of exp * exp * exp     (* if statement *)
    | And of exp * exp          (* logical and *)
    | Or of exp * exp           (* logical or *)
    | Apply of exp * exp        (* primitive procedure or closure *)
    | Call of exp * exp list    (* primitive procedure or closure *)
    | Lambda of name list * exp (* lambda *)
    | Defexp of def             (* modifies env *)
and def =
    | Val of name * exp
    | Def of name * name list * exp
    | Exp of exp

exception SyntaxErr of string
exception ParseError of string
exception TypeError of string
exception ThisCan'tHappenError

let rec pair_to_list pr =
    match pr with
    | Nil -> []
    | Pair (a, b) -> a::(pair_to_list b)
    | _ -> raise ThisCan'tHappenError

let rec is_list pr =
    match pr with
    | Nil -> true
    | Pair (_, b) -> is_list b
    | _ -> false

let rec env_to_val =
    let b_to_val (n, vor) =
        Pair (Symbol n, (
            match !vor with
            | Some v -> v
            | None -> Symbol "unspecified"
        ))
    in
    function
    | [] -> Nil
    | b::bs -> Pair(b_to_val b, env_to_val bs)
