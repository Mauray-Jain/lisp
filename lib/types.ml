type lobject =
    | Fixnum of int
    | Boolean of bool
    | Symbol of string
    | Nil
    | Pair of lobject * lobject

exception SyntaxErr of string
exception ThisCan'tHappenError

let rec pair_to_list pr =
    match pr with
    | Nil -> []
    | Pair(a,b) -> a::(pair_to_list b)
    | _ -> raise ThisCan'tHappenError

let rec is_list pr =
    match pr with
    | Nil -> true
    | Pair(_,b) -> is_list b
    | _ -> false
