(* TODO(dlsmith): Should these tests live under `test_interpreter`?
   Including scanning and parsing for convenience seemed ok because of the
   types: there was no way to produce statements to evaluate without parsing
   and no way to produce tokens to parse without scanning. But now that we're
   implicitly including analysis passes it seems less clean--more likely to
   lead to confusion or bugs. *)

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
        |> (fun (_, v) -> v))

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

let test_logical_and () =
    let source = "\"first\" and \"second\"" in
    Alcotest.(check literal_testable)
        "Produces second value"
        (Ast.String "second")
        (source
        |> evaluate_expr
        |> Util.get_ok
        |> (fun (_, v) -> v))

let test_logical_and_short_circuits () =
    (* Evaluating the undefined variable would cause an error. *)
    let source = "nil and undefined" in
    Alcotest.(check literal_testable)
        "Produces first value"
        Ast.Nil
        (source
        |> evaluate_expr
        |> Util.get_ok
        |> (fun (_, v) -> v))

let test_logical_or () =
    let source = "nil or \"second\"" in
    Alcotest.(check literal_testable)
        "Produces second value"
        (Ast.String "second")
        (source
        |> evaluate_expr
        |> Util.get_ok
        |> (fun (_, v) -> v))

let test_logical_or_short_circuits () =
    (* Evaluating the undefined variable would cause an error. *)
    let source = "\"truthy\" or undefined" in
    Alcotest.(check literal_testable)
        "Produces first value"
        (Ast.String "truthy")
        (source
        |> evaluate_expr
        |> Util.get_ok
        |> (fun (_, v) -> v))

let run_interpreter source =
    let env = Env.make ~parent:None in
    match Interpreter.run env source with
    | Ok (_env, value) -> Ok value
    | Error _ as e -> e

let test_clock_increases () =
    let source = "
        var start = clock();
        // Do some work to make sure we have enough time to detect.
        for (var i = 0; i < 1000; i = i + 1) i + i;
        clock() - start > 0;" in
    Alcotest.(check literal_testable)
        "Is true"
        Ast.True
        (source |> run_interpreter |> Util.get_ok)

let test_function_arity_mismatch () =
    let source = "fun f (a) { 2 * a; } f();" in
    Alcotest.(check string)
        "Produces arity error"
        "[line 0] Error: Expected 1 arguments but got 0"
        (source |> run_interpreter |> Result.get_error)

let test_function_returns_nil_by_default () =
    let source = "fun f (a, b) { a + b; } f(1, 2);" in
    Alcotest.(check literal_testable)
        "Produces nil"
        Ast.Nil
        (source |> run_interpreter |> Util.get_ok)

let test_does_not_execute_statements_after_return () =
    let source = "
        var global = \"unmodified\";
        fun f () {
            var a = \"inside\";

            // Exercises `return` propagation.
            if (1. + 2. > 0.)
                return a;

            global = \"modified\";
            return a;
        }
        f() + \";\" + global;" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "inside;unmodified")
        (source |> run_interpreter |> Util.get_ok)

let test_return_breaks_from_loop () =
    let source = "
        fun f () {
            for (var i = 0; i < 10; i = i + 1) {
                if (i > 5)
                    return i;
            }
        }
        f();" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.Number 6.)
        (source |> run_interpreter |> Util.get_ok)

let test_can_recurse () =
    let source = "
        fun f (s) {
            if (s == \"\") {
                return f(\"called\");
            } else {
                return s;
            }
        }
        f(\"\");" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "called")
        (source |> run_interpreter |> Util.get_ok)

let test_closure () =
    let source = "
        fun makeCounter() {
            var i = 0;
            fun count() {
                i = i + 1;
                return i;
            }

            // The closure should capture the value of `i` at declaration.
            i = 1000;

            return count;
        }

        var counter = makeCounter();
        counter();
        counter();" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.Number 2.)
        (source |> run_interpreter |> Util.get_ok)

let test_return_prohibited_outside_of_function () =
    let source = "return \"at top level\";" in
    Alcotest.(check (result literal_testable string))
        "Has error"
        (Error "Can't return from top-level code.")
        (source |> run_interpreter)

let test_simple_program_with_variable_declaration () =
    let source = "var a = \"one\" + \"two\"; a + \"three\";" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "onetwothree")
        (source |> run_interpreter |> Util.get_ok)

let test_simple_program_with_variable_assignment () =
    let source = "var a; a = 1.; a > 0.;" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.True)
        (source |> run_interpreter |> Util.get_ok)

let test_outer_scope_is_preserved_during_shadowing () =
    let source = "var a = \"outer\"; { var a = \"inner\"; } a;" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "outer")
        (source |> run_interpreter |> Util.get_ok)

let test_outer_scope_is_modified_by_assignment () =
    let source = "var a = \"outer\"; { var b = 1 + 2; a = \"inner\"; } a;" in
    Alcotest.(check literal_testable)
        "Same value"
        (Ast.String "inner")
        (source |> run_interpreter |> Util.get_ok)

let test_inner_scope_decl_not_available_in_outer () =
    let source = "{ var a = \"inner\"; } a;" in
    Alcotest.(check string)
        "Same value"
        "[line 0] Error: Undefined variable 'a'."
        (source |> run_interpreter |> Result.get_error)

let test_final_value_not_returned_if_within_a_block () =
    let source = "{ 1 + 2; }" in
    Alcotest.(check (result literal_testable string))
        "Nil result"
        (Ok Nil)
        (source |> run_interpreter)

let test_multiple_child_scopes () =
    let source = "var a = 1.; { a = a + 1.; } { a = 2 * a; } a >= 4.;" in
    Alcotest.(check (result literal_testable string))
        "Expected value"
        (Ok Ast.True)
        (source |> run_interpreter)

let test_else_not_executed_for_true_condition () =
    let source = "var a = 3.; if (true) a = a * 5.; else a = a + 1.; a;" in
    Alcotest.(check (result literal_testable string))
        "Expected value"
        (Ok (Ast.Number 15.))
        (source |> run_interpreter)

let test_simple_program_with_while_loop () =
    let source = "var a = 0.; while (a < 5.) a = a + 1.; a;" in
    Alcotest.(check (result literal_testable string))
        "Expected value"
        (Ok (Ast.Number 5.))
        (source |> run_interpreter)

let test_for_iter_var_not_accessible_outside_loop () =
    let source = "
        for (var i = 0; i < 10; i = i + 1) {
            i;
        }
        i;" in
    Alcotest.(check (result literal_testable string))
        "Expected error"
        (Error "[line 4] Error: Undefined variable 'i'.")
        (source |> run_interpreter)

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
                Alcotest.test_case
                    "Logical and"
                    `Quick
                    test_logical_and;
                Alcotest.test_case
                    "Logical and short-circuits"
                    `Quick
                    test_logical_and_short_circuits;
                Alcotest.test_case
                    "Logical or"
                    `Quick
                    test_logical_or;
                Alcotest.test_case
                    "Logical or short-circuits"
                    `Quick
                    test_logical_or_short_circuits;
            ]);
            ("Native functions", [
                Alcotest.test_case
                    "Clock result increases"
                    `Quick
                    test_clock_increases;
            ]);
            ("Functions", [
                Alcotest.test_case
                    "Errors for arity mismatch"
                    `Quick
                    test_function_arity_mismatch;
                Alcotest.test_case
                    "Returns nil by default"
                    `Quick
                    test_function_returns_nil_by_default;
                Alcotest.test_case
                    "Does not execute statements after return"
                    `Quick
                    test_does_not_execute_statements_after_return;
                Alcotest.test_case
                    "Return breaks from loop"
                    `Quick
                    test_return_breaks_from_loop;
                Alcotest.test_case
                    "Can recurse"
                    `Quick
                    test_can_recurse;
                Alcotest.test_case
                    "Closure"
                    `Quick
                    test_closure;
                Alcotest.test_case
                    "Return prohibited outside of function"
                    `Quick
                    test_return_prohibited_outside_of_function;
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
                Alcotest.test_case
                    "Simple program with while loop"
                    `Quick
                    test_simple_program_with_while_loop;
                Alcotest.test_case
                    "For iter var is not accessible outside loop"
                    `Quick
                    test_for_iter_var_not_accessible_outside_loop;
            ]);
        ]
