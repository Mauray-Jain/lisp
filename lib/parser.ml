open Types
open Stream

let string_of_char c =
    String.make 1 c

let is_symstartchar = function
    | '*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+' -> true
    | c -> is_alpha c

let rec read_sexp stm =
    let rec read_fixnum stm acc =
        let nc = read_char stm in
        if is_digit nc then
            read_fixnum stm (acc ^ Char.escaped nc)
        else
            let _ = unread_char stm nc in
            Fixnum (int_of_string acc)
    in

    let rec read_symbol stm =
        let is_delimiter = function
            | '(' | ')' | '{' | '}' | ';' | '"' -> true
            | c -> is_whitespace c
        in
        let nc = read_char stm in
        if is_delimiter nc then
            let _ = unread_char stm nc in ""
        else
            string_of_char nc ^ read_symbol stm
    in

    let rec read_list stm =
        eat_whitespace stm;
        let c = read_char stm in
        if c = ')' then
            Nil
        else
            let _ = unread_char stm c in
            let car = read_sexp stm in
            let cdr = read_list stm in
            Pair (car, cdr)
    in

    let rec eat_comment stm =
        let c = read_char stm in
        if c = '\n' then () else eat_comment stm
    in

    eat_whitespace stm;
    let c = read_char stm in
    let peek_char = read_char stm in
    unread_char stm peek_char;

    if is_digit c || (c = '-' && (is_digit peek_char)) then
        read_fixnum stm (Char.escaped c)
    else if c = '#' then
        match read_char stm with
        | 't' -> Boolean (true)
        | 'f' -> Boolean (false)
        | x -> raise (SyntaxErr ("Invalid boolean literal #" ^ (Char.escaped x)))
    else if is_symstartchar c then
        Symbol (string_of_char c ^ read_symbol stm)
    else if c = '(' then
        read_list stm
    else if c = '\'' then
        Quote (read_sexp stm)
    else if c = ';' then
        let () = eat_comment stm in
        read_sexp stm
    else
        raise (SyntaxErr ("Unexpected character: " ^ (Char.escaped c)))
