let (let*) = Result.bind

let run source =
    let token_results =
        source
        |> Scanner.scan_tokens
        (* TODO(dlsmith): Consume `Seq` directly. *)
        |> List.of_seq in
    let* tokens =
        match List.map Result.get_ok token_results with
        (* TODO(dlsmith): Surface all errors. *)
        | exception Invalid_argument message -> Error message
        | tokens -> Ok tokens
    in
    let* expr, remaining_tokens =
        Parser.parse_expression tokens
        (* TODO(dlsmith): Surface all errors. *)
        |> Result.map_error (fun (message, _) -> message)
    in
    match remaining_tokens with
    | [ { token_type=Token.EOF; _} ] ->
        let output_str =
            match Parser.evaluate_expression expr with
            | Ok literal -> Parser.to_sexp (Parser.Literal literal)
            | Error message -> message
        in
        print_endline output_str;
        Ok ()
    | _ -> Error "Expected a single expression"

let report line where message =
    Printf.printf "[line %i] Error %s: %s" line where message

let error line message =
    report line "" message
