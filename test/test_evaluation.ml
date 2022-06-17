let literal_testable =
    Alcotest.testable Parsing.pp_literal Parsing.equal_literal

let evaluate_expr source =
    let (let*) = Result.bind in
    let* tokens = Interpreter.scan_or_error source in
    let* expr, rest_tokens =
        Parsing.parse_expression tokens
        (* TODO(dlsmith): Surface all errors. *)
        |> Result.map_error (fun (message, _) -> message)
    in
    match rest_tokens with
    | [ { token_type=Token.EOF; _} ] ->
        Evaluation.(evaluate_expression (Env.make()) expr)
    | _ -> Error "Expected a single expression"

let test_evaluate_valid_expression () =
    let source = "1. + 2. > 0." in
    Alcotest.(check literal_testable)
        "Same value"
        (source
        |> evaluate_expr
        |> Util.get_ok
        |> (fun (v, _) -> v))
        Parsing.True

let test_evaluate_invalid_negation () =
    let source = "-\"hello\"" in
    Alcotest.(check string)
        "Same value"
        (source |> evaluate_expr |> Result.get_error)
        "[line 0] Error: Operand must be a number."

let test_evaluate_invalid_sum () =
    let source = "1. + \"hello\"" in
    Alcotest.(check string)
        "Same value"
        (source |> evaluate_expr |> Result.get_error)
        "[line 0] Error: Operands must be two numbers or two strings."

let test_evaluate_invalid_operands () =
    let source = "\"one\" / \"two\"" in
    Alcotest.(check string)
        "Same value"
        (source |> evaluate_expr |> Result.get_error)
        "[line 0] Error: Operands must be numbers."

let test_simple_program_with_variable_declaration () =
    let source = "var a = \"one\" + \"two\"; a + \"three\";" in
    Alcotest.(check literal_testable)
        "Same value"
        (source |> Interpreter.run |> Util.get_ok |> Option.get)
        (Parsing.String "onetwothree")

let test_simple_program_with_variable_assignment () =
    let source = "var a; a = 1.; a > 0.;" in
    Alcotest.(check literal_testable)
        "Same value"
        (source |> Interpreter.run |> Util.get_ok |> Option.get)
        (Parsing.True)

let () =
    Alcotest.run "Evaluation test suite"
        [
            ("Expressions", [
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
            ("Program", [
                Alcotest.test_case
                "Simple program with variable declaration"
                `Quick
                test_simple_program_with_variable_declaration;
                Alcotest.test_case
                "Simple program with variable assignment"
                `Quick
                test_simple_program_with_variable_assignment;
            ]);
        ]
