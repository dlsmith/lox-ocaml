let read_file path =
    let in_channel = open_in path in
    let channel_length = in_channel_length in_channel in
    let contents = really_input_string in_channel channel_length in
    close_in in_channel;
    contents

let interpret_file path =
    path
    |> read_file
    |> Interpreter.run (Env.make ~parent:None)
    |> Result.map (fun (_, value) -> value)  (* Discard env *)
    |> Result.map Ast.literal_to_string
    |> Result.map Option.some

let interpret_interactive () =
    let rec interpret_user_input env =
        print_string "> ";
        try
            let line = read_line() in
            match Interpreter.run env line with
            | Ok (updated_env, output) ->
                let print_literal = fun value ->
                    print_endline (Ast.literal_to_string value) in
                let _ = print_literal output in
                interpret_user_input updated_env
            | Error message ->
                print_endline message;
                interpret_user_input env
        with End_of_file ->
            Ok None
    in

    interpret_user_input (Env.make ~parent:None)
