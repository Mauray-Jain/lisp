open Lisp

let rec repl stm env =
    print_string "> ";
    flush stdout;
    let sexp = Parser.read_sexp stm in
    let (result, env') = Evaler.eval_sexp sexp env in
    Types.print_sexp result;
    print_newline ();
    repl stm env'

let () =
    let stm: Stream.stream = { chr=[]; line_num=1; chan=stdin } in
    repl stm Evaler.basis
