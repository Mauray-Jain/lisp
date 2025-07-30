open Lisp
open Types

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

let rec repl stm =
    print_string "> ";
    flush stdout;
    let sexp = Parser.read_sexp stm in
    print_sexp sexp;
    print_newline ();
    repl stm;;

let () =
    let stm: Stream.stream = { chr=[]; line_num=1; chan=stdin } in
    repl stm;;
