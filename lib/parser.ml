let string_of_char c =
    String.make 1 c;;

let rec read_fixnum stm acc =
    let nc = Stream.read_char stm in
    if Stream.is_digit nc then
        read_fixnum stm (acc ^ Char.escaped nc)
    else
        let _ = Stream.unread_char stm nc in
        Types.Fixnum (int_of_string acc);;

let is_symstartchar = function
    | '*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+' -> true
    | c -> Stream.is_alpha c

let rec read_symbol stm =
    let is_delimiter = function
        | '(' | ')' | '{' | '}' | ';' | '"' -> true
        | c -> Stream.is_whitespace c
    in
    let nc = Stream.read_char stm in
    if is_delimiter nc then
        let _ = Stream.unread_char stm nc in ""
    else
        string_of_char nc ^ read_symbol stm

let read_sexp stm =
    Stream.eat_whitespace stm;
    let c = Stream.read_char stm in
    let peek_char = Stream.read_char stm in
    Stream.unread_char stm peek_char;
    if Stream.is_digit c || (c = '-' && (Stream.is_digit peek_char)) then
        read_fixnum stm (Char.escaped c)
    else if c = '#' then
        match Stream.read_char stm with
        | 't' -> Types.Boolean (true)
        | 'f' -> Types.Boolean (false)
        | x -> raise (Types.SyntaxErr ("Invalid boolean literal #" ^ (Char.escaped x)))
    else if is_symstartchar c then
        Types.Symbol (string_of_char c ^ read_symbol stm)
    else
        raise (Types.SyntaxErr ("Unexpected character: " ^ (Char.escaped c)));;
