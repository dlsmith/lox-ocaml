let usage_msg = "loxi [script]"

let input_paths = ref []

let speclist = []

let anon_fun path =
    match !input_paths with
    | [] -> input_paths := path :: !input_paths
    | _ :: _ -> raise (Arg.Bad "Expected at most one positional argument")

let handle_result result =
    match result with
    | Ok _ -> exit(0)
    | Error message ->
        print_endline message; exit(65)

let () =
    Arg.parse speclist anon_fun usage_msg;
    match List.hd !input_paths with
    | path -> path |> Entrypoint.interpret_file |> handle_result
    | exception Failure _ ->
        Entrypoint.interpret_interactive () |> handle_result

