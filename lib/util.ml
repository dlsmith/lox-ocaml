let head = function
    | [] -> None
    | [x] -> Some x
    | x::_ -> Some x

let tail = function
    | [] -> []
    | [_] -> []
    | _::xs -> xs

let uncons = function
    | [] -> None, []
    | [x] -> Some x, []
    | x::xs -> Some x, xs

(** `Result.get_ok` that actually surfaces the underlying error. *)
let get_ok = function
    | Ok v -> v
    | Error e -> raise (Invalid_argument e)
