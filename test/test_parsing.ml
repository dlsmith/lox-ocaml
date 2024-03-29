let clean_sexp s =
    s
    (* TODO(dlsmith): Clean this up or use s-exp comparison that ignores
       whitespace. *)
    |> Str.global_replace (Str.regexp "[ \t\n]+") " "
    |> Str.global_replace (Str.regexp "( ") "("
    |> Str.global_replace (Str.regexp " )") ")"
    |> String.trim

let check_parse_expr_ok tokens expected_sexp =
    match Parsing.parse_expression tokens with
    | Ok (expr, tokens) ->
        Alcotest.(check int)
            "No tokens remaining"
            0
            (List.length tokens);
        Alcotest.(check string)
            "Same s-exp"
            expected_sexp
            (Ast.expr_to_sexp expr)

    | _ -> Alcotest.fail "Expected parse ok"

let check_parse_expr_error tokens expected_message =
    match Parsing.parse_expression tokens with
    | Error (message, _) ->
        Alcotest.(check string)
            "Same error"
            expected_message
            message
    | _ -> Alcotest.fail "Expected parse error"

let check_parse_stmt_ok tokens expected_sexp =
    match Parsing.parse_statement tokens with
    | Ok (stmt, [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same string"
            expected_sexp
            (Ast.stmt_to_sexp stmt)
    | _ -> Alcotest.fail "Expected parse ok"

let check_parse_stmt_error tokens expected_error =
    match Parsing.parse_statement tokens with
    | Error (message, [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same error"
            expected_error
            message
    | Error (_, []) -> Alcotest.fail "Expected EOF token to remain"
    | _ -> Alcotest.fail "Expected parsing error"

let create_tokens token_types =
    let type_to_token tt = Token.{ token_type=tt; line=0; } in
    List.map type_to_token token_types

let test_arithmetic_precedence () =
    let open Token in
    let tokens = create_tokens [Number 1.; Plus; Number 2.; Star; Number 3.] in
    check_parse_expr_ok tokens "(+ 1. (* 2. 3.))"

let test_unary_binary_grouping () =
    let open Token in
    let tokens = create_tokens [
        LeftParen; Number 1.; Plus; Number 2.; RightParen; Star;
        Minus; Number 3.; Less; Number 0.; EqualEqual; True;
    ] in
    check_parse_expr_ok tokens "(== (< (* (group (+ 1. 2.)) (- 3.)) 0.) true)"

let test_unclosed_grouping () =
    let open Token in
    let tokens = create_tokens [
        Number 1.; Plus; LeftParen; Number 2.; Star; Number 3.;
    ] in
    check_parse_expr_error tokens "Expect ')' after expression."

let test_partial_binary_expression () =
    let open Token in
    let tokens = create_tokens [Number 1.; Plus] in
    check_parse_expr_error tokens "Expect expression."

let test_token_not_consumed_on_error () =
    let token_testable =
        Alcotest.testable Token.pp_token Token.equal_token in
    let open Token in
    let tokens = create_tokens [Number 1.; Plus; EOF] in
    match Parsing.parse_expression tokens with
    | Error (message, rest_tokens) ->
        Alcotest.(check string)
            "Same error"
            "Expect expression."
            message;
        Alcotest.(check (list token_testable))
            "Same tokens"
            [ { token_type=EOF; line=0; } ]
            rest_tokens
    | _ -> Alcotest.fail "Expected parse error"

let test_assignment () =
    let open Token in
    let tokens = create_tokens [
        Identifier "a"; Equal; Number 2.; Star; Number 3.;
    ] in
    check_parse_expr_ok tokens "(assign a (* 2. 3.))"

let test_group_is_invalid_assignment_target () =
    let open Token in
    let tokens = create_tokens [
        LeftParen; Identifier "a"; RightParen; Equal;
        Number 2.; Star; Number 3.;
    ] in
    check_parse_expr_error tokens "Invalid assignment target."

let test_binary_expr_is_invalid_assignment_target () =
    let open Token in
    let tokens = create_tokens [
        Identifier "a"; Plus; Identifier "a"; Equal; Number 2.;
    ] in
    check_parse_expr_error tokens "Invalid assignment target."

let test_parse_nested_call () =
    let expected = clean_sexp "
        (call
            (call
                (call
                    (var callee)
                    ())
                ((var one)))
            (2. (+ 1. 2.)))" in
    let open Token in
    let tokens = create_tokens [
        Identifier "callee";
        LeftParen; RightParen;
        LeftParen; Identifier "one"; RightParen;
        LeftParen; Number 2.; Comma; Number 1.; Plus; Number 2.; RightParen;
    ] in
    check_parse_expr_ok tokens expected

let test_parse_incomplete_statement () =
    let open Token in
    let tokens = create_tokens [
        Number 1.; Plus; Number 2.; (* No semicolon *) EOF;
    ] in
    check_parse_stmt_error tokens "Expect ';' after value."

let test_parse_incomplete_block () =
    let open Token in
    let tokens = create_tokens [
        LeftBrace; Number 1.; Plus; Number 2.; Semicolon; (* No brace *) EOF;
    ] in
    check_parse_stmt_error tokens "Expect '}' after block."

let test_block_statement () =
    let var_name = "a" in
    let expected = "(block ((var-decl a 2.) (print (+ 1. (var a)))))" in
    let open Token in
    let tokens = create_tokens [
        LeftBrace;
        (* var a = 2.; *)
        Var; Identifier var_name; Equal; Number 2.; Semicolon;
        (* print 1. + a; *)
        Print; Number 1.; Plus; Identifier var_name; Semicolon;
        RightBrace;
        EOF;
    ] in
    check_parse_stmt_ok tokens expected

let test_else_bound_to_nearest_if () =
    let expected = "(if true (if false (expr 1.) (expr 2.)))" in
    let open Token in
    let tokens = create_tokens [
        If; LeftParen; True; RightParen;
        If; LeftParen; False; RightParen;
        Number 1.; Semicolon;
        Else; Number 2.; Semicolon;
        EOF;
    ] in
    check_parse_stmt_ok tokens expected

let test_for_loop_desugaring () =
    let expected = clean_sexp "
        (block (
            (var-decl i 0.)
            (while
                (< (var i) 5.)
                (block (
                    (print (var i))
                    (expr
                        (assign
                            i
                            (+ (var i) 1.))))))))" in

    let open Token in
    let tokens = create_tokens [
        For;
        LeftParen;
        (* Initializer *)
        Var; Identifier "i"; Equal; Number 0.; Semicolon;
        (* Condition *)
        Identifier "i"; Less; Number 5.; Semicolon;
        (* Increment *)
        Identifier "i"; Equal; Identifier "i"; Plus; Number 1.;
        RightParen;
        (* Body *)
        Print; Identifier "i"; Semicolon;
        EOF
    ] in
    check_parse_stmt_ok tokens expected

let test_function_declaration () =
    let expected = clean_sexp "
        (fun-decl
            print_sum
            (a b)
            ((print (+ (var a) (var b)))))" in
    let open Token in
    let tokens = create_tokens [
        Fun; Identifier "print_sum";
        LeftParen; Identifier "a"; Comma; Identifier "b"; RightParen;
        LeftBrace;
        Print; Identifier "a"; Plus; Identifier "b"; Semicolon; 
        RightBrace;
        EOF;
    ] in
    match Parsing.parse_declaration tokens with
    | Ok (stmt, [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same string"
            expected
            (Ast.stmt_to_sexp stmt)
    | _ -> Alcotest.fail "Expected parse ok"

let test_parse_multiple_statements () =
    let var_name = "a" in
    let expected =
        "((var-decl a 2.) (expr (+ 1. (var a))) (print (- 3. 4.)))" in
    let open Token in
    let tokens = create_tokens [
        (* var a = 2. ; *)
        Var; Identifier var_name; Equal; Number 2.; Semicolon;
        (* 1. + a; *)
        Number 1.; Plus; Identifier var_name; Semicolon;
        (* print 3. + 4.; *)
        Print; Number 3.; Minus; Number 4.; Semicolon;
        EOF;
    ] in
    let stmts = Parsing.parse_program tokens |> List.map Result.get_ok in
    Alcotest.(check string)
        "Same string"
        expected
        (Ast.stmts_to_sexp stmts)

let test_recovers_from_error () =
    let open Token in
    let tokens = create_tokens [
        (* Error from missing paren before conditional. Should resume after
           termination of the then branch print statement.

            if true
                a = "skipped";

            print "after";
        *)
        If; True; Identifier "a"; Equal; String "skipped";
        Print; String "after"; Semicolon;
        EOF;
    ] in
    match Parsing.parse_program tokens with
    | [Error "Expect '(' after 'if'."; Ok stmt] ->
        Alcotest.(check string)
            "Same string"
            "(print \"after\")"
            (Ast.stmt_to_sexp stmt)
    | _ -> Alcotest.fail "Unexpected program parse"

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
                Alcotest.test_case
                    "Nested call"
                    `Quick
                    test_parse_nested_call;
            ]);
            ("Parse statements", [
                Alcotest.test_case
                    "Incomplete statement"
                    `Quick
                    test_parse_incomplete_statement;
                Alcotest.test_case
                    "Incomplete block"
                    `Quick
                    test_parse_incomplete_block;
                Alcotest.test_case
                    "Block statement"
                    `Quick
                    test_block_statement;
                Alcotest.test_case
                    "Else bound to nearest if"
                    `Quick
                    test_else_bound_to_nearest_if;
                Alcotest.test_case
                    "For loop desugaring"
                    `Quick
                    test_for_loop_desugaring;
                Alcotest.test_case
                    "Function declaration"
                    `Quick
                    test_function_declaration;
            ]);
            ("Parse program", [
                Alcotest.test_case
                    "Multiple statements"
                    `Quick
                    test_parse_multiple_statements;
                Alcotest.test_case
                    "Recovers from error"
                    `Quick
                    test_recovers_from_error;
            ]);
        ]
