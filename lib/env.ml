module Table = struct
    include Map.Make(String)

    let pp _ fmt _ = Format.pp_print_string fmt "Map<string, 'a>"
end

(* TODO(dlsmith): Can we do this without mutability (i.e., refs)? *)
type 'a t = {
    values: 'a Table.t ref;
    parent: 'a t ref option
}
[@@deriving show, eq]

let make ~parent = {values=ref Table.empty; parent=parent}

let contains t key = Table.mem key !(t.values)

let get_parent env = env.parent

let define env key value =
    env.values := Table.add key value !(env.values);
    env

let rec set env key value =
    let (let*) = Result.bind in
    if contains env key
        then Ok (define env key value)
        else match env.parent with
        | Some parent_ref ->
            let* updated_parent = set !parent_ref key value in
            parent_ref := updated_parent;
            Ok env
        | None -> Error ("Undefined variable '" ^ key ^ "'.")

let rec get env key =
    match Table.find_opt key !(env.values), env.parent with
    | Some v, _ -> Ok v
    | None, Some env_ref -> get !env_ref key
    | None, None -> Error ("Undefined variable '" ^ key ^ "'.")

let shallow_copy env = { env with values = ref !(env.values) }
