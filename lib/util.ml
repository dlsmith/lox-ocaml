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

(* Return a complete list of values or the subset of errors.
   If any errors are present then all values are discarded. *)
let to_values_or_errors (rs: ('a, 'e) result list) : ('a list, 'e list) result =
    let rec collect vs es = function
        | [] ->
            begin match es with
            | [] -> Ok (List.rev vs)
            | _ -> Error (List.rev es)
            end
        (* Below, we stop collecting values if we see any errors. *)
        | (Ok v) :: rs ->
            begin match es with
            | [] -> collect (v :: vs) es rs
            | _ -> collect [] es rs
            end
        | (Error e) :: rs ->
            collect [] (e :: es) rs in

    collect [] [] rs


let concat_errors = function
    | Ok _ as a -> a
    | Error messages -> Error (String.concat "\n" messages)
