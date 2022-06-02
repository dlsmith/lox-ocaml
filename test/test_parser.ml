let test_print_ast () =
    let expected = "(== (> 1.2 (- (group (+ 5. 2.)))) true)" in
    let ast =
        let open Parser in
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
                                Literal (Number 2.0)
                            )))),
                Literal True)
        in
    Alcotest.(check string)
        "Same string"
        expected
        (Parser.print_ast ast)

let test_arithmetic_precedence () =
    let open Token in
    let tokens = [
        { token_type=Number 1.; lexeme="1.0"; line=0; };
        { token_type=Plus; lexeme="+"; line=0; };
        { token_type=Number 2.; lexeme="2.0"; line=0; };
        { token_type=Star; lexeme="*"; line=0; };
        { token_type=Number 3.; lexeme="3.0"; line=0; };
    ] in
    (* TODO(dlsmith): Check all tokens consumed. *)
    let expr, _ = Parser.parse_expression tokens in
    let expected_sexp = "(+ 1. (* 2. 3.))" in
    Alcotest.(check string)
        "Same string"
        expected_sexp
        (Parser.print_ast expr)

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
    (* TODO(dlsmith): Check all tokens consumed. *)
    let expr, _ = Parser.parse_expression tokens in
    let expected_sexp = "(== (< (* (group (+ 1. 2.)) (- 3.)) 0.) true)" in
    Alcotest.(check string)
        "Same string"
        expected_sexp
        (Parser.print_ast expr)

let () =
    Alcotest.run "Parser test suite"
        [
            ("Print AST", [
                Alcotest.test_case
                "Print AST"
                `Quick
                test_print_ast;
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
        ]
