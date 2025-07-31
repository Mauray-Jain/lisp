open Types

exception NotFound of string

let rec lookup (n, e) =
    match e with
    | Nil -> raise (NotFound n)
    | Pair(Pair(Symbol n', v), rst) ->
        if n = n' then v else lookup (n, rst)
    | _ -> raise ThisCan'tHappenError

let bind (n, v, e) = Pair(Pair(Symbol n, v), e)
