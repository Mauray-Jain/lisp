type lobject =
    | Fixnum of int
    | Boolean of bool

exception SyntaxErr of string
