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

let () =
    Alcotest.run "Parsing test suite"
        [
            ("Print AST", [
                Alcotest.test_case
                "Print AST"
                `Quick
                test_to_sexp;
            ]);
            ("Parse expression", [
                Alcotest.test_case
                "Arithmetic precedence"
                `Quick
                test_arithmetic_precedence;
                Alcotest.test_case
                "Expression with unary, binary, and grouping"
                `Quick
                test_unary_binary_grouping;
            ]);
            ("Parse failure", [
                Alcotest.test_case
                "Unclosed grouping"
                `Quick
                test_unclosed_grouping;
                Alcotest.test_case
                "Partial binary expression"
                `Quick
                test_partial_binary_expression;
            ]);
        ]
