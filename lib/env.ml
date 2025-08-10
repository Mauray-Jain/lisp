exception NotFound of string
exception UnspecifiedValue of string

type 'a env = (string * 'a option ref) list

let bind (n, v, e) = (n, ref (Some v))::e

let mkloc () = ref None

let bindloc : string * 'a option ref * 'a env -> 'a env = fun (n, vor, e) -> (n, vor)::e

let bindlist ns vs env =
    List.fold_left2 (fun acc n v -> bind (n, v, acc)) env ns vs

let rec lookup = function
    | (n, []) -> raise (NotFound n)
    | (n, (n', v)::_) when n = n' ->
        begin
            match !v with
            | Some v -> v
            | None -> raise (UnspecifiedValue n)
        end
    | (n, (_, _)::e) -> lookup (n, e)

let extend oldenv newenv =
    List.fold_right (fun (b, v) acc -> bindloc (b, v, acc)) newenv oldenv
