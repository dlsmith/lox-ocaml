let (let*) = Result.bind

let head = function
    | [] -> None
    | [x] -> Some x
    | x :: _ -> Some x

let tail = function
    | [] -> []
    | [_] -> []
    | _ :: xs -> xs

(** `Result.get_ok` that actually surfaces the underlying error. *)
let get_ok = function
    | Ok v -> v
    | Error e -> raise (Invalid_argument e)

(** Map a `Result` producing function, short-circuting early on `Error`. *)
let rec map_or_error f = function
    | [] -> Ok ()
    | [x] -> f x
    | x :: xs ->
        let* _ = f x in
        map_or_error f xs
