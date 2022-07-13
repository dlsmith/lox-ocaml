let usage_msg = "loxi [script]"

let input_paths = ref []

let speclist = []

let anon_fun path =
    match !input_paths with
    | [] -> input_paths := path :: !input_paths
    | _ :: _ -> raise (Arg.Bad "Expected at most one positional argument")

let () =
    Arg.parse speclist anon_fun usage_msg;
    match List.hd !input_paths with
    | path ->
        begin match path |> Entrypoint.interpret_file |> Util.concat_errors with
        | Ok output -> let _ = Option.map print_endline output in exit(0)
        | Error message -> print_endline message; exit(65)
        end
    | exception Failure _ ->
        Entrypoint.interpret_interactive ();
        exit(0)
