let check_parse tokens ~ok ~error =
    let partial_parse = Parsing.parse_expression tokens in
    match partial_parse with
    | Ok (expr, tokens) -> ok (expr, tokens)
    | Error (message, tokens) -> error (message, tokens)

let check_parse_error tokens expected_message =
    check_parse
        tokens
        ~ok:(fun _ -> Alcotest.fail "Expected parse error")
        ~error:(fun (message, _) ->
            Alcotest.(check string)
            "Same error"
            expected_message
            message
        )

let check_parse_ok tokens expected_sexp =
    check_parse
        tokens
        ~ok:(fun (expr, tokens) ->
            Alcotest.(check int)
                "No tokens remaining"
                0
                (List.length tokens);
            Alcotest.(check string)
                "Same s-exp"
                expected_sexp
                (Ast.to_sexp expr)
        )
        ~error:(fun _ -> Alcotest.fail "Expected parse ok")

let create_tokens token_types =
    token_types |> List.map (fun tt ->
        Token.{ token_type=tt; line=0; })

let test_arithmetic_precedence () =
    let open Token in
    let tokens = create_tokens [Number 1.; Plus; Number 2.; Star; Number 3.] in
    check_parse_ok tokens "(+ 1. (* 2. 3.))"

let test_unary_binary_grouping () =
    let open Token in
    let tokens = create_tokens [
        LeftParen; Number 1.; Plus; Number 2.; RightParen; Star;
        Minus; Number 3.; Less; Number 0.; EqualEqual; True;
    ] in
    check_parse_ok tokens "(== (< (* (group (+ 1. 2.)) (- 3.)) 0.) true)"

let test_unclosed_grouping () =
    let open Token in
    let tokens = create_tokens [
        Number 1.; Plus; LeftParen; Number 2.; Star; Number 3.;
    ] in
    check_parse_error tokens "Expect ')' after expression."

let test_partial_binary_expression () =
    let open Token in
    let tokens = create_tokens [Number 1.; Plus] in
    check_parse_error tokens "Expect expression."

let test_token_not_consumed_on_error () =
    let token_testable =
        Alcotest.testable Token.pp_token Token.equal_token in
    let open Token in
    let tokens = create_tokens [Number 1.; Plus; EOF] in
    check_parse
        tokens
        ~ok:(fun _ -> Alcotest.fail "Expected parse error")
        ~error:(fun (message, rest_tokens) ->
            Alcotest.(check string)
                "Same error"
                "Expect expression."
                message;
            Alcotest.(check (list token_testable))
                "Same tokens"
                [ { token_type=EOF; line=0; } ]
                rest_tokens
        )

let test_assignment () =
    let open Token in
    let tokens = create_tokens [
        Identifier "a"; Equal; Number 2.; Star; Number 3.;
    ] in
    check_parse_ok tokens "(assign a (* 2. 3.))"

let test_group_is_invalid_assignment_target () =
    let open Token in
    let tokens = create_tokens [
        LeftParen; Identifier "a"; RightParen; Equal;
        Number 2.; Star; Number 3.;
    ] in
    check_parse_error tokens "Invalid assignment target."

let test_binary_expr_is_invalid_assignment_target () =
    let open Token in
    let tokens = create_tokens [
        Identifier "a"; Plus; Identifier "a"; Equal; Number 2.;
    ] in
    check_parse_error tokens "Invalid assignment target."

let test_parse_print_statement () =
    let open Token in
    let expected_expr = "(+ 1. 2.)" in
    let tokens = create_tokens [
        Print; Number 1.; Plus; Number 2.; Semicolon; EOF;
    ] in
    match Parsing.parse_statement tokens with
    | Ok (Ast.Print expr, [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same string"
            expected_expr
            (Ast.to_sexp expr)
    | Error (message, _) -> Alcotest.fail ("Parsing error: " ^ message)
    | _ -> Alcotest.fail "Unexpected parse"

let test_parse_var_declaration () =
    let open Token in
    let var_name = "some_name" in
    let expected_init_expr = "(+ 1. 2.)" in
    let tokens = create_tokens [
        Var; Identifier var_name; Equal;
        Number 1.; Plus; Number 2.; Semicolon; EOF;
    ]
    in
    match Parsing.parse_declaration tokens with
    | Ok (Ast.VariableDeclaration (name, Some init_expr),
          [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same string"
            var_name
            name;
        Alcotest.(check string)
            "Same string"
            expected_init_expr
            (Ast.to_sexp init_expr)
    | Error (message, _) -> Alcotest.fail ("Parsing error: " ^ message)
    | _ -> Alcotest.fail "Unexpected parse"

let test_parse_incomplete_statement () =
    let open Token in
    let tokens = create_tokens [
        Number 1.; Plus; Number 2.; (* No semicolon *) EOF;
    ] in
    match Parsing.parse_statement tokens with
    | Error (message, [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same error"
            "Expect ';' after value."
            message
    | Error (_, []) -> Alcotest.fail "Expected EOF token to remain"
    | _ -> Alcotest.fail "Expected parsing error"

let test_parse_multiple_statements () =
    let open Token in
    let expected_expr1 = "(+ 1. 2.)" in
    let expected_expr2 = "(- 3. 4.)" in
    let tokens = create_tokens [
        Number 1.; Plus; Number 2.; Semicolon;
        Print; Number 3.; Minus; Number 4.; Semicolon; EOF;
    ] in
    let stmt_results = Parsing.parse_program tokens in
    (* TODO(dlsmith): Refactor to clean this up a bit, so we're not pattern
       matching down to expressions which we then serialize. E.g., could add
       support for full programs in `to_sexp`. *)
    match stmt_results with
    | [ Ok (Expression expr1); Ok (Print expr2) ] ->
        Alcotest.(check string)
            "Same string"
            expected_expr1
            (Ast.to_sexp expr1);
        Alcotest.(check string)
            "Same string"
            expected_expr2
            (Ast.to_sexp expr2)
    | _ -> Alcotest.fail "Unexpected parse"

let () =
    Alcotest.run "Parsing test suite"
        [
            ("Parse expressions", [
                Alcotest.test_case
                "Arithmetic precedence"
                `Quick
                test_arithmetic_precedence;
                Alcotest.test_case
                "Expression with unary, binary, and grouping"
                `Quick
                test_unary_binary_grouping;
                Alcotest.test_case
                "Unclosed grouping"
                `Quick
                test_unclosed_grouping;
                Alcotest.test_case
                "Partial binary expression"
                `Quick
                test_partial_binary_expression;
                Alcotest.test_case
                "Token not consumed on error"
                `Quick
                test_token_not_consumed_on_error;
                Alcotest.test_case
                "Assignment"
                `Quick
                test_assignment;
                Alcotest.test_case
                "Group is invalid assignment target"
                `Quick
                test_group_is_invalid_assignment_target;
                Alcotest.test_case
                "Binary expr is invalid assignment target"
                `Quick
                test_binary_expr_is_invalid_assignment_target;
            ]);
            ("Parse statements", [
                Alcotest.test_case
                "Print statement"
                `Quick
                test_parse_print_statement;
                Alcotest.test_case
                "Incomplete statement"
                `Quick
                test_parse_incomplete_statement;
                Alcotest.test_case
                "Variable declaration"
                `Quick
                test_parse_var_declaration;
                (* TODO(dlsmith): Unintialized var and error cases. *)
                (* TODO(dlsmith): Synchronization. *)
                (* TODO(dlsmith): Block parsing. This can be part of the
                   refactor to better support statements in general, rather
                   than pattern matching down to the inner expression. *)
            ]);
            ("Parse program", [
                Alcotest.test_case
                "Multiple statements"
                `Quick
                test_parse_multiple_statements;
            ]);
        ]
