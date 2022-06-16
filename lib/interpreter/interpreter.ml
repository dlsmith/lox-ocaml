let scan_or_error source =
    try
        source
        |> Scanning.scan_tokens
        (* TODO(dlsmith): Consume `Seq` directly in parsing? *)
        |> List.of_seq
        (* TODO(dlsmith): Surface all errors. *)
        |> List.map Util.get_ok
        |> Result.ok
    with Invalid_argument message -> Error message

let run source =
    let (let*) = Result.bind in
    let* tokens = scan_or_error source in
    try
        tokens
        |> Parsing.parse_program
        (* TODO(dlsmith): Surface all errors. *)
        |> List.map Util.get_ok
        |> Evaluation.(evaluate_program (Env.make()))
    with Invalid_argument message -> Error message

let report line where message =
    Printf.printf "[line %i] Error %s: %s" line where message

let error line message =
    report line "" message
