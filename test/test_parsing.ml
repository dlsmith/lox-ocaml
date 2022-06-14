let test_to_sexp () =
    let expected = "(== (> 1.2 (- (group (+ 5. 2.)))) true)" in
    let ast =
        let open Parsing in
            Binary (
                EqualEqual,
                Binary (
                    Greater,
                    Literal (Number 1.2),
                    Unary (
                        Negate,
                        Grouping (
                            Binary (
                                Plus,
                                Literal (Number 5.0),
                                Literal (Number 2.0),
                                LineNumber 0
                            )),
                        LineNumber 0),
                        LineNumber 0
                    ),
                Literal True,
                LineNumber 0)
        in
    Alcotest.(check string)
        "Same string"
        expected
        (Parsing.to_sexp ast)

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
                (Parsing.to_sexp expr)
        )
        ~error:(fun _ -> Alcotest.fail "Expected parse ok")

let test_arithmetic_precedence () =
    let open Token in
    let tokens = [
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        { token_type=Star; lexeme="*"; line=0; };
        { token_type=Number 3.; lexeme="3.0"; line=0; };
    ] in
    check_parse_ok tokens "(+ 1. (* 2. 3.))"

let test_unary_binary_grouping () =
    let open Token in
    let tokens = [
        { token_type=LeftParen; lexeme="("; line=0; };
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        { token_type=RightParen; lexeme="("; line=0; };
        { token_type=Star; lexeme="*"; line=0; };
        { token_type=Minus; lexeme="-"; line=0; };
        { token_type=Number 3.; lexeme="3.0"; line=0; };
        { token_type=Less; lexeme="<"; line=0; };
        { token_type=Number 0.; lexeme="0.0"; line=0; };
        { token_type=EqualEqual; lexeme="=="; line=0; };
        { token_type=True; lexeme="true"; line=0; };
    ] in
    check_parse_ok tokens "(== (< (* (group (+ 1. 2.)) (- 3.)) 0.) true)"

let test_unclosed_grouping () =
    let open Token in
    let tokens = [
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=LeftParen; lexeme="("; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        { token_type=Star; lexeme="*"; line=0; };
        { token_type=Number 3.; lexeme="3.0"; line=0; };
    ] in
    check_parse_error tokens "Expect ')' after expression."

let test_partial_binary_expression () =
    let open Token in
    let tokens = [
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
    ] in
    check_parse_error tokens "Expect expression."

let test_token_not_consumed_on_error () =
    let token_testable =
        Alcotest.testable Token.pp_token Token.equal_token in
    let open Token in
    let tokens = [
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=EOF; lexeme=""; line=0; };
    ] in
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
                [ { token_type=EOF; lexeme=""; line=0; } ]
                rest_tokens
        )

let test_parse_print_statement () =
    let open Token in
    let expected_expr = "(+ 1. 2.)" in
    let tokens = [
        { token_type=Print; lexeme="print"; line=0; };
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        { token_type=Semicolon; lexeme=";"; line=0; };
        { token_type=EOF; lexeme=""; line=0; };
    ] in
    match Parsing.parse_statement tokens with
    | Ok (Parsing.Print expr, [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same string"
            expected_expr
            (Parsing.to_sexp expr)
    | Error (message, _) -> Alcotest.fail ("Parsing error: " ^ message)
    | _ -> Alcotest.fail "Unexpected parse"

let test_parse_var_declaration () =
    let open Token in
    let var_name = "some_name" in
    let expected_init_expr = "(+ 1. 2.)" in
    let tokens = [
        { token_type=Var; lexeme="var"; line=0; };
        { token_type=Identifier var_name; lexeme=var_name; line=0; };
        { token_type=Equal; lexeme="="; line=0; };
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        { token_type=Semicolon; lexeme=";"; line=0; };
        { token_type=EOF; lexeme=""; line=0; };
    ]
    in
    match Parsing.parse_declaration tokens with
    | Ok (Parsing.VariableDeclaration (name, Some init_expr),
          [ { token_type=EOF; _ } ]) ->
        Alcotest.(check string)
            "Same string"
            var_name
            name;
        Alcotest.(check string)
            "Same string"
            expected_init_expr
            (Parsing.to_sexp init_expr)
    | Error (message, _) -> Alcotest.fail ("Parsing error: " ^ message)
    | _ -> Alcotest.fail "Unexpected parse"

let test_parse_incomplete_statement () =
    let open Token in
    let tokens = [
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        (* No semicolon *)
        { token_type=EOF; lexeme=""; line=0; };
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
    let tokens = [
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        { token_type=Semicolon; lexeme=";"; line=0; };
        { token_type=Print; lexeme="print"; line=0; };
        { token_type=Number 3.; lexeme="3.0"; line=0; };
        { token_type=Minus; lexeme="-"; line=0; };
        { token_type=Number 4.; lexeme="4.0"; line=0; };
        { token_type=Semicolon; lexeme=";"; line=0; };
        { token_type=EOF; lexeme=""; line=0; };
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
            (Parsing.to_sexp expr1);
        Alcotest.(check string)
            "Same string"
            expected_expr2
            (Parsing.to_sexp expr2)
    | _ -> Alcotest.fail "Unexpected parse"

let () =
    Alcotest.run "Parsing test suite"
        [
            ("Print AST", [
                Alcotest.test_case
                "Print AST"
                `Quick
                test_to_sexp;
            ]);
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
            ]);
            ("Parse program", [
                Alcotest.test_case
                "Multiple statements"
                `Quick
                test_parse_multiple_statements;
            ]);
        ]
