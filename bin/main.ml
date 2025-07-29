open Lisp

let print_sexp s =
    match s with
    | Types.Fixnum(num) -> print_int num
    | Types.Boolean(b) -> print_string (if b then "#t" else "#f")
    | Types.Symbol(str) -> print_string str;;

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
