open Lisp

let rec repl stm =
    print_string "> ";
    flush stdout;
    let Types.Fixnum(v) = Parser.read_sexp stm in
    print_string "Your int: ";
    print_int v;
    print_newline ();
    repl stm;;

let () =
    let stm: Stream.stream = { chr=[]; line_num=1; chan=stdin } in
    repl stm;;
