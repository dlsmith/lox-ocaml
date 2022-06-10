let (let*) = Result.bind

let run source =
    let* expr = Util.scan_and_parse source in
    let* literal = Parsing.evaluate_expression expr in
    print_endline (Parsing.to_sexp (Parsing.Literal literal));
    Ok ()

let report line where message =
    Printf.printf "[line %i] Error %s: %s" line where message

let error line message =
    report line "" message
