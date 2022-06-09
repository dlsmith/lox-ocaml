let (let*) = Result.bind

(* TODO(dlsmith): This is just a hacky composition of the scanner and parser to
   enable the top-level entrypoint and testing. The full interpreter will get
   much more fleshed out over time, especially with respect to error handling.
 *)
let scan_and_parse source =
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
    | [ { token_type=Token.EOF; _} ] -> Ok expr
    | _ -> Error "Expected a single expression"
