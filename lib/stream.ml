type stream = {
    mutable line_num: int;
    mutable chr: char list;
    chan: in_channel;
};;

let read_char stm =
    match stm.chr with
    | [] ->
        let c = input_char stm.chan in
        if c = '\n' then let _ = stm.line_num <- stm.line_num + 1 in c
        else c
    | c::rest ->
        let _ = stm.chr <- rest in c;;

let unread_char stm c =
    stm.chr <- c :: stm.chr;;

let is_whitespace c =
    c = ' ' || c = '\n' || c = '\t';;

let is_digit c =
    '0' <= c && c <= '9';;

let is_alpha = function
    | 'A'..'Z' | 'a'..'z' -> true
    | _ -> false;;

let rec eat_whitespace stm =
    let c = read_char stm in
    if is_whitespace c then
        eat_whitespace stm
    else
        unread_char stm c;
        ();;
