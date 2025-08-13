open Lisp

let rec repl (stm: Stream.stream) env =
    if stm.chan = stdin then ( print_string "> "; flush stdout; );
    let ast = Parser.read_sexp stm |> Ast.build_ast in
    let (result, env') = Evaler.eval ast env in
    if stm.chan = stdin then print_endline (Ast.string_val result);
    repl stm env'

let get_ic () =
    try open_in Sys.argv.(1)
    with Invalid_argument _ -> stdin

let () =
    let ic = get_ic () in
    let stm: Stream.stream = { chr=[]; line_num=1; chan=ic } in
    try repl stm Evaler.basis
    with End_of_file -> if ic <> stdin then close_in ic
