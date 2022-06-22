let test_to_sexp () =
    let expected = "(== (> 1.2 (- (group (+ 5. 2.)))) true)" in
    let ast =
        let open Ast in
            Binary (
                EqualEqual,
                Binary (
                    Greater,
                    Literal (Number 1.2, LineNumber 0),
                    Unary (
                        Negate,
                        Grouping (
                            Binary (
                                Plus,
                                Literal (Number 5.0, LineNumber 0),
                                Literal (Number 2.0, LineNumber 0),
                                LineNumber 0
                            ),
                            LineNumber 0),
                        LineNumber 0),
                        LineNumber 0
                    ),
                Literal (True, LineNumber 0),
                LineNumber 0)
        in
    Alcotest.(check string)
        "Same string"
        expected
        (Ast.expr_to_sexp ast)

let () =
    Alcotest.run "Parsing test suite"
        [
            ("Serialize to s-exp", [
                Alcotest.test_case
                "Nested expression"
                `Quick
                test_to_sexp;
            ]);
        ]
