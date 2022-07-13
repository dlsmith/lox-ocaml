type env_of_literals = Evaluation.env_of_literals

let scan_or_error source =
    source
    |> Scanning.scan_tokens
    (* TODO(dlsmith): Consume `Seq` directly in parsing? *)
    |> List.of_seq
    |> Util.to_values_or_errors

let run env source =
    let (let*) = Result.bind in
    let* tokens = scan_or_error source in
    let* stmts =
        tokens
        |> Parsing.parse_program
        |> Util.to_values_or_errors in
    let* stmts =
        stmts
        |> Analysis.analyze_return
        |> Util.to_values_or_errors in
    (* `run` can in general return multiple errors. *)
    Evaluation.evaluate_program env stmts
    |> Result.map_error (fun e -> [e])
