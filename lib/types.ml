type lobject =
    | Fixnum of int
    | Boolean of bool
    | Symbol of string
    | Nil
    | Pair of lobject * lobject
    | Primitive of string * (lobject list -> lobject)
    | Quote of value
and value = lobject
and name = string
and exp =
    | Literal of value       (* self evaluating *)
    | Var of name            (* variable name *)
    | If of exp * exp * exp  (* if statement *)
    | And of exp * exp       (* logical and *)
    | Or of exp * exp        (* logical or *)
    | Apply of exp * exp     (* primitive procedure or closure *)
    | Call of exp * exp list (* primitive procedure or closure *)
    | Defexp of def          (* modifies env *)
and def =
    | Val of name * exp
    | Exp of exp

exception SyntaxErr of string
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
