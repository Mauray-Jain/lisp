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
    | Pair(a, b) -> a::(pair_to_list b)
    | _ -> raise ThisCan'tHappenError

let rec is_list pr =
    match pr with
    | Nil -> true
    | Pair(_, b) -> is_list b
    | _ -> false

let rec print_sexp s =
    let rec print_list l =
        match l with
        | Pair(a, Nil) -> print_sexp a
        | Pair(a, b) -> print_sexp a; print_char ' '; print_list b
        | _ -> raise ThisCan'tHappenError
    in
    let print_pair p =
        match p with
        | Pair(a, b) -> print_sexp a; print_string " . "; print_sexp b
        | _ -> raise ThisCan'tHappenError
    in
    match s with
    | Fixnum(num) -> print_int num
    | Boolean(b) -> print_string (if b then "#t" else "#f")
    | Symbol(str) -> print_string str
    | Nil -> print_string "nil"
    | Pair(_, _) ->
        print_string "(";
        if is_list s then
            print_list s
        else
            print_pair s;
        print_string ")";;

