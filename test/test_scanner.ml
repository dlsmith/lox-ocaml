let token_testable =
    Alcotest.testable Token.pp_token Token.equal_token

let token_type_testable =
    Alcotest.testable Token.pp_token_type Token.equal_token_type

let position_testable =
    Alcotest.testable Scanner.pp_position Scanner.equal_position

let token_result_list_testable =
    Alcotest.(list (result token_testable string))

exception Scanner_error of string

let unpack_scanner_result result =
    match result with
    | Ok value -> value
    | Error message -> raise (Scanner_error message)

let scan_token_result source =
    let start_pos = Scanner.init () in
    let token_result, _ = Scanner.scan_and_extract_token source start_pos in
    unpack_scanner_result token_result

let check_scan_token_eq source ~expected =
    let token = scan_token_result source in
    Alcotest.(check token_testable)
        "Same token" expected token

let test_parse_single_char_token () =
    check_scan_token_eq "(" ~expected:Token.{
        token_type=Token.LeftParen;
        lexeme="(";
        literal=None;
        line=0
    }

let test_parse_unexpected_char () =
    Alcotest.check_raises
        "Scanner error"
        (Scanner_error "Unexpected character.")
        (fun () -> let _ = scan_token_result "@" in ())

let test_parse_single_char_with_whitespace () =
    check_scan_token_eq "  (" ~expected:Token.{
        token_type=Token.LeftParen;
        lexeme="(";
        literal=None;
        line=0
    }

let test_newline_increments_line () =
    check_scan_token_eq "\n;" ~expected:Token.{
        token_type=Token.Semicolon;
        lexeme=";";
        literal=None;
        line=1
    }

let test_parse_single_char_after_comment () =
    check_scan_token_eq "// This is a comment\n{" ~expected:Token.{
        token_type=Token.LeftBrace;
        lexeme="{";
        literal=None;
        line=1
    }

let test_slash_token_without_comment () =
    check_scan_token_eq "/4" ~expected:Token.{
        token_type=Token.Slash;
        lexeme="/";
        literal=None;
        line=0
    }

let test_multi_char_token () =
    check_scan_token_eq "!=" ~expected:Token.{
        token_type=Token.BangEqual;
        lexeme="!=";
        literal=None;
        line=0
    }

let test_keyword_and_identifier_with_overlap () =
    check_scan_token_eq "or" ~expected:Token.{
        token_type=Token.Or;
        lexeme="or";
        literal=None;
        line=0
    };
    check_scan_token_eq "orchid" ~expected:Token.{
        token_type=Token.Identifier;
        lexeme="orchid";
        literal=None;
        line=0
    }

let test_parse_string_literal () =
    check_scan_token_eq "\"hello world\"" ~expected:Token.{
        token_type=Token.String;
        lexeme="\"hello world\"";
        literal=Some (Literal.String "hello world");
        line=0
    }

let test_handle_unterminated_string () =
    Alcotest.check_raises
        "Scanner error"
        (Scanner_error "Unterminated string.")
        (fun () -> let _ = scan_token_result "\"hello world" in ())

let test_unterminated_string_has_correct_position () =
    let source = "\"hello world" in
    let start_pos = Scanner.init () in
    let token_type_result, pos = Scanner.scan_token source start_pos in
    match token_type_result with
    | Ok _ -> Alcotest.fail "String should not have parsed successfully"
    | Error message ->
        Alcotest.(check string)
            "Same error"
            "Unterminated string."
            message;
        Alcotest.(check position_testable)
            "Same position"
            Scanner.{ start=0; current=String.length source; line=0 }
            pos

let test_parse_int_literal () =
    check_scan_token_eq "123" ~expected:Token.{
        token_type=Token.Number;
        lexeme="123";
        literal=Some (Literal.Number 123.0);
        line=0
    }

let test_parse_float_literal () =
    check_scan_token_eq "123.4" ~expected:Token.{
        token_type=Token.Number;
        lexeme="123.4";
        literal=Some (Literal.Number 123.4);
        line=0
    }

let test_parse_float_literal_without_fractional () =
    check_scan_token_eq "123." ~expected:Token.{
        token_type=Token.Number;
        lexeme="123.";
        literal=Some (Literal.Number 123.0);
        line=0
    }

let test_produces_EOF_at_end () =
    check_scan_token_eq "" ~expected:Token.{
        token_type=Token.EOF;
        lexeme="";
        literal=None;
        line=0
    }

let test_no_change_after_EOF () =
    let source = "" in
    let pos0 = Scanner.init () in
    let token_result1, pos1 = Scanner.scan_token source pos0 in
    let token_result2, pos2 = Scanner.scan_token source pos1 in
    Alcotest.(check token_type_testable)
        "EOF type"
        Token.EOF
        (unpack_scanner_result token_result1);
    Alcotest.(check token_type_testable)
        "EOF type"
        Token.EOF
        (unpack_scanner_result token_result2);
    Alcotest.(check position_testable)
        "Same position" pos1 pos2

let test_multi_token_scan () =
    let source =
        "var v = 1.2;\n\n// Check.\nif (v >= 0) {\n\tprint \"t\";\n}" in
    let some_number num = Some (Literal.Number num) in
    let some_string str = Some (Literal.String str) in
    let open Token in
    let expected_tokens = [
        { token_type=Var; lexeme="var"; literal=None; line=0; };
        { token_type=Identifier; lexeme="v"; literal=None; line=0; };
        { token_type=Equal; lexeme="="; literal=None; line=0; };
        { token_type=Number; lexeme="1.2"; literal=some_number 1.2; line=0; };
        { token_type=Semicolon; lexeme=";"; literal=None; line=0; };
        { token_type=If; lexeme="if"; literal=None; line=3; };
        { token_type=LeftParen; lexeme="("; literal=None; line=3; };
        { token_type=Identifier; lexeme="v"; literal=None; line=3; };
        { token_type=GreaterEqual; lexeme=">="; literal=None; line=3; };
        { token_type=Number; lexeme="0"; literal=some_number 0.0; line=3; };
        { token_type=RightParen; lexeme=")"; literal=None; line=3; };
        { token_type=LeftBrace; lexeme="{"; literal=None; line=3; };
        { token_type=Print; lexeme="print"; literal=None; line=4; };
        { token_type=String; lexeme="\"t\""; literal=some_string "t"; line=4; };
        { token_type=Semicolon; lexeme=";"; literal=None; line=4; };
        { token_type=RightBrace; lexeme="}"; literal=None; line=5; };
        { token_type=EOF; lexeme=""; literal=None; line=5; };
    ] in
    let tokens =
        source
        |> Scanner.scan_tokens
        |> Seq.map Result.get_ok
        |> List.of_seq in
    Alcotest.(check (list token_testable))
        "Tokens equal"
        expected_tokens
        tokens

let test_captures_multiple_errors () =
    let source = "(#=@)" in
    let open Token in
    let expected_token_results = [
        Ok { token_type=LeftParen; lexeme="("; literal=None; line=0; };
        Error "Unexpected character.";
        Ok { token_type=Equal; lexeme="="; literal=None; line=0; };
        Error "Unexpected character.";
        Ok { token_type=RightParen; lexeme=")"; literal=None; line=0; };
        Ok { token_type=EOF; lexeme=""; literal=None; line=0; };
    ] in
    let token_results = source |> Scanner.scan_tokens |> List.of_seq in
    Alcotest.(check token_result_list_testable)
        "Token results equal"
        expected_token_results
        token_results

let () =
    Alcotest.run "Scanner test suite"
        [
            ("Single char token", [
                Alcotest.test_case
                "Parse single char token"
                `Quick
                test_parse_single_char_token;
                Alcotest.test_case
                "Failure for unexpected char"
                `Quick
                test_parse_unexpected_char;
                Alcotest.test_case
                "Parse single char with whitespace"
                `Quick
                test_parse_single_char_with_whitespace;
                Alcotest.test_case
                "Newline increments line"
                `Quick
                test_newline_increments_line;
                Alcotest.test_case
                "Parse single char after comment"
                `Quick
                test_parse_single_char_after_comment;
                Alcotest.test_case
                "Slash token without comment"
                `Quick
                test_slash_token_without_comment;
            ]);
            ("Multi char token", [
                Alcotest.test_case
                "Parse multi-char token"
                `Quick
                test_multi_char_token;
                Alcotest.test_case
                "Parse keyword and identifier with overlap"
                `Quick
                test_keyword_and_identifier_with_overlap;
            ]);
            ("Literals", [
                Alcotest.test_case
                "Parse string literal"
                `Quick
                test_parse_string_literal;
                Alcotest.test_case
                "Handle unterminated string"
                `Quick
                test_handle_unterminated_string;
                Alcotest.test_case
                "Unterminated string has correct position"
                `Quick
                test_unterminated_string_has_correct_position;
                Alcotest.test_case
                "Parse int literal"
                `Quick
                test_parse_int_literal;
                Alcotest.test_case
                "Parse float literal"
                `Quick
                test_parse_float_literal;
                Alcotest.test_case
                "Parse float literal without fractional"
                `Quick
                test_parse_float_literal_without_fractional;
            ]);
            ("Termination cases", [
                Alcotest.test_case
                "Produces EOF at end"
                `Quick
                test_produces_EOF_at_end;
                Alcotest.test_case
                "No change after EOF"
                `Quick
                test_no_change_after_EOF;
            ]);
            ("Multi token scan", [
                Alcotest.test_case
                "Multiple tokens"
                `Quick
                test_multi_token_scan;
                Alcotest.test_case
                "Captures multiple errors"
                `Quick
                test_captures_multiple_errors;
            ]);
        ]
