let rec read_fixnum stm acc =
    let nc = Stream.read_char stm in
    if Stream.is_digit nc then
        read_fixnum stm (acc ^ Char.escaped nc)
    else
        let _ = Stream.unread_char stm nc in
        Types.Fixnum (int_of_string acc)

let read_sexp stm =
    Stream.eat_whitespace stm;
    let c = Stream.read_char stm in
    if Stream.is_digit c then
        read_fixnum stm (Char.escaped c)
    else
        raise (Types.SyntaxErr ("Unexpected character: " ^ (Char.escaped c)));;
