let read_file path =
    let in_channel = open_in path in
    let channel_length = in_channel_length in_channel in
    let contents = really_input_string in_channel channel_length in
    close_in in_channel;
    contents

let interpret_file path =
    path |> read_file |> Interpreter.run

let rec interpret_interactive () =
    print_string "> ";
    try
        let line = read_line() in
        let result = Interpreter.run line in
        match result with
        | Ok () -> interpret_interactive()
        | Error message ->
            print_endline message;
            interpret_interactive()
    with End_of_file ->
        Ok ()
