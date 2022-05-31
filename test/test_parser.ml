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

let () =
    Alcotest.run "Parser test suite"
        [
            ("Print AST", [
                Alcotest.test_case
                "Print AST"
                `Quick
                test_print_ast;
            ]);
        ]
