module Env = Evaluation.Env

let literal_testable =
    Alcotest.testable Ast.pp_literal Ast.equal_literal

let test_contains_with_empty_env () =
    let env = Env.make ~parent:None in
    Alcotest.(check bool)
        "Does not contain"
        false
        (Env.contains env "a")

let test_set_fails_for_undefined () =
    let env = Env.make ~parent:None in
    match Env.set env "a" Ast.True with
    | Ok _ -> Alcotest.fail "Expected `set` to fail"
    | Error message ->
        Alcotest.(check string)
            "Same error"
            "Undefined variable 'a'."
            message

let test_set_in_parent_scope () =
    let parent = Env.make ~parent:None in
    let parent = Env.define parent "a" (Ast.String "parent") in
    let child = Env.make ~parent:(Some (ref parent)) in
    let child = Env.set child "a" (Ast.String "child") |> Result.get_ok in
    Alcotest.(check bool)
        "Child does not contain"
        false
        (Env.contains child "a");
    Alcotest.(check bool)
        "Parent does contain"
        true
        (Env.contains parent "a");
    Alcotest.(check literal_testable)
        "Parent contains value set via child"
        (Ast.String "child")
        (Env.get parent "a" |> Result.get_ok)

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
        Evaluation.(evaluate_expression (Env.make ~parent:None) expr)
    | _ -> Error "Expected a single expression"

let test_evaluate_valid_expression () =
    let source = "1. + 2. > 0." in
    Alcotest.(check literal_testable)
        "Same value"
        Ast.True
        (source
        |> evaluate_expr
        |> Util.get_ok
        |> (fun (v, _) -> v))

let test_evaluate_invalid_negation () =
    let source = "-\"hello\"" in
    Alcotest.(check string)
        "Same value"
        "[line 0] Error: Operand must be a number."
        (source |> evaluate_expr |> Result.get_error)

let test_evaluate_invalid_sum () =
    let source = "1. + \"hello\"" in
    Alcotest.(check string)
        "Same value"
        "[line 0] Error: Operands must be two numbers or two strings."
        (source |> evaluate_expr |> Result.get_error)

let test_evaluate_invalid_operands () =
    let source = "\"one\" / \"two\"" in
    Alcotest.(check string)
        "Same value"
        "[line 0] Error: Operands must be numbers."
        (source |> evaluate_expr |> Result.get_error)

let test_simple_program_with_variable_declaration () =
    let source = "var a = \"one\" + \"two\"; a + \"three\";" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "onetwothree")
        (source |> Interpreter.run |> Util.get_ok |> Option.get)

let test_simple_program_with_variable_assignment () =
    let source = "var a; a = 1.; a > 0.;" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.True)
        (source |> Interpreter.run |> Util.get_ok |> Option.get)

let test_outer_scope_is_preserved_during_shadowing () =
    let source = "var a = \"outer\"; { var a = \"inner\"; } a;" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "outer")
        (source |> Interpreter.run |> Util.get_ok |> Option.get)

let test_outer_scope_is_modified_by_assignment () =
    let source = "var a = \"outer\"; { var b = 1 + 2; a = \"inner\"; } a;" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "inner")
        (source |> Interpreter.run |> Util.get_ok |> Option.get)

let test_inner_scope_decl_not_available_in_outer () =
    let source = "{ var a = \"inner\"; } a;" in
    Alcotest.(check string)
        "Same value"
        "[line 0] Error: Undefined variable 'a'."
        (source |> Interpreter.run |> Result.get_error)

let test_final_value_not_returned_if_within_a_block () =
    let source = "{ 1 + 2; }" in
    Alcotest.(check (result (option literal_testable) string))
        "Unit result"
        (Ok None)
        (source |> Interpreter.run)

let test_multiple_child_scopes () =
    let source = "var a = 1.; { a = a + 1.; } { a = 2 * a; } a >= 4.;" in
    Alcotest.(check (result (option literal_testable) string))
        "Expected value"
        (Ok (Some Ast.True))
        (source |> Interpreter.run)

let test_else_not_executed_for_true_condition () =
    let source = "var a = 3.; if (true) a = a * 5.; else a = a + 1.; a;" in
    Alcotest.(check (result (option literal_testable) string))
        "Expected value"
        (Ok (Some (Ast.Number 15.)))
        (source |> Interpreter.run)

let () =
    Alcotest.run "Evaluation test suite"
        [
            ("Env", [
                Alcotest.test_case
                    "Contains with empty Env"
                    `Quick
                    test_contains_with_empty_env;
                Alcotest.test_case
                    "Set fails for undefined"
                    `Quick
                    test_set_fails_for_undefined;
                Alcotest.test_case
                    "Set in parent scope"
                    `Quick
                    test_set_in_parent_scope;
            ]);
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
                Alcotest.test_case
                    "Outer scope is preserved during shadowing"
                    `Quick
                    test_outer_scope_is_preserved_during_shadowing;
                Alcotest.test_case
                    "Outer scope is modified by assignment"
                    `Quick
                    test_outer_scope_is_modified_by_assignment;
                Alcotest.test_case
                    "Inner scope declaration is not available in outer"
                    `Quick
                    test_inner_scope_decl_not_available_in_outer;
                Alcotest.test_case
                    "Final value not returned if within a block"
                    `Quick
                    test_final_value_not_returned_if_within_a_block;
                Alcotest.test_case
                    "Multiple child scopes"
                    `Quick
                    test_multiple_child_scopes;
                Alcotest.test_case
                    "Else not executed for true condition"
                    `Quick
                    test_else_not_executed_for_true_condition;
            ]);
        ]
