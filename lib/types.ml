type lobject =
    | Fixnum of int
    | Boolean of bool
    | Symbol of string

exception SyntaxErr of string
