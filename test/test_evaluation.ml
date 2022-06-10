let literal_testable =
    Alcotest.testable Parsing.pp_literal Parsing.equal_literal

let evaluate source =
    match Util.scan_and_parse source with
    | Ok expr -> Evaluation.evaluate_expression expr
    | Error message ->
        Alcotest.fail (Printf.sprintf "Unexpected failure: %s" message)

let test_evaluate_valid_expression () =
    let source = "1. + 2. > 0." in
    Alcotest.(check literal_testable)
        "Same value"
        (source |> evaluate |> Result.get_ok)
        Parsing.True

let test_evaluate_invalid_negation () =
    let source = "-\"hello\"" in
    Alcotest.(check string)
        "Same value"
        (source |> evaluate |> Result.get_error)
        "[line 0] Error: Operand must be a number."

let test_evaluate_invalid_sum () =
    let source = "1. + \"hello\"" in
    Alcotest.(check string)
        "Same value"
        (source |> evaluate |> Result.get_error)
        "[line 0] Error: Operands must be two numbers or two strings."

let test_evaluate_invalid_operands () =
    let source = "\"one\" / \"two\"" in
    Alcotest.(check string)
        "Same value"
        (source |> evaluate |> Result.get_error)
        "[line 0] Error: Operands must be numbers."

let () =
    Alcotest.run "Evaluation test suite"
        [
            ("Evaluation", [
                Alcotest.test_case
                "Valid expression"
                `Quick
                test_evaluate_valid_expression;
                Alcotest.test_case
                "Invalid negation"
                `Quick
                test_evaluate_invalid_negation;
                Alcotest.test_case
                "Invalid operands for sum"
                `Quick
                test_evaluate_invalid_sum;
                Alcotest.test_case
                "Non-numeric operands"
                `Quick
                test_evaluate_invalid_operands;
            ]);
        ]
