type env_of_literals = Evaluation.env_of_literals

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

let run env source =
    let (let*) = Result.bind in
    let* tokens = scan_or_error source in
    try
        tokens
        |> Parsing.parse_program
        (* TODO(dlsmith): Surface all errors. *)
        |> List.map Util.get_ok
        |> Analysis.analyze_return
        |> List.map Util.get_ok
        |> Evaluation.(evaluate_program env)
    with Invalid_argument message -> Error message