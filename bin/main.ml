open Lisp

let rec repl stm env =
    print_string "> ";
    flush stdout;
    let ast = Parser.read_sexp stm |> Ast.build_ast in
    let (result, env') = Evaler.eval ast env in
    print_endline (Ast.string_val result);
    repl stm env'

let () =
    let stm: Stream.stream = { chr=[]; line_num=1; chan=stdin } in
    repl stm Evaler.basis
