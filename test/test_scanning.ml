let token_testable =
    Alcotest.testable Token.pp_token Token.equal_token

let token_type_testable =
    Alcotest.testable Token.pp_token_type Token.equal_token_type

let position_testable =
    Alcotest.testable Scanning.pp_position Scanning.equal_position

let token_result_list_testable =
    Alcotest.(list (result token_testable string))

exception Scanning_error of string

let raise_on_error = function
    | Ok (value, pos) -> value, pos
    | Error (message, _) -> raise (Scanning_error message)

let scan_token source =
    let start_pos = Scanning.init () in
    let token, _ = 
        Scanning.scan_and_extract_token source start_pos
        |> raise_on_error in
    token

let check_scan_token_eq source ~expected =
    let token = scan_token source in
    Alcotest.(check token_testable)
        "Same token" expected token

let test_parse_single_char_token () =
    check_scan_token_eq "(" ~expected:Token.{
        token_type=Token.LeftParen;
        line=0
    }

let test_parse_unexpected_char () =
    Alcotest.check_raises
        "Scanning error"
        (Scanning_error "Unexpected character.")
        (fun () -> let _ = scan_token "@" in ())

let test_parse_single_char_with_whitespace () =
    check_scan_token_eq "  (" ~expected:Token.{
        token_type=Token.LeftParen;
        line=0
    }

let test_newline_increments_line () =
    check_scan_token_eq "\n;" ~expected:Token.{
        token_type=Token.Semicolon;
        line=1
    }

let test_parse_single_char_after_comment () =
    check_scan_token_eq "// This is a comment\n{" ~expected:Token.{
        token_type=Token.LeftBrace;
        line=1
    }

let test_slash_token_without_comment () =
    check_scan_token_eq "/4" ~expected:Token.{
        token_type=Token.Slash;
        line=0
    }

let test_multi_char_token () =
    check_scan_token_eq "!=" ~expected:Token.{
        token_type=Token.BangEqual;
        line=0
    }

let test_keyword_and_identifier_with_overlap () =
    check_scan_token_eq "or" ~expected:Token.{
        token_type=Token.Or;
        line=0
    };
    check_scan_token_eq "orchid" ~expected:Token.{
        token_type=Token.Identifier "orchid";
        line=0
    }

let test_parse_string_literal () =
    check_scan_token_eq "\"hello world\"" ~expected:Token.{
        token_type=Token.String "hello world";
        line=0
    }

let test_handle_unterminated_string () =
    Alcotest.check_raises
        "Scanning error"
        (Scanning_error "Unterminated string.")
        (fun () -> let _ = scan_token "\"hello world" in ())

let test_unterminated_string_has_correct_position () =
    let source = "\"hello world" in
    let start_pos = Scanning.init () in
    match Scanning.scan_token source start_pos with
    | Ok _ -> Alcotest.fail "String should not have parsed successfully"
    | Error (message, pos) ->
        Alcotest.(check string)
            "Same error"
            "Unterminated string."
            message;
        Alcotest.(check position_testable)
            "Same position"
            Scanning.{ start=0; current=String.length source; line=0 }
            pos

let test_parse_int_literal () =
    check_scan_token_eq "123" ~expected:Token.{
        token_type=Token.Number 123.0;
        line=0
    }

let test_parse_float_literal () =
    check_scan_token_eq "123.4" ~expected:Token.{
        token_type=Token.Number 123.4;
        line=0
    }

let test_parse_float_literal_without_fractional () =
    check_scan_token_eq "123." ~expected:Token.{
        token_type=Token.Number 123.0;
        line=0
    }

let test_produces_EOF_at_end () =
    check_scan_token_eq "" ~expected:Token.{
        token_type=Token.EOF;
        line=0
    }

let test_no_change_after_EOF () =
    let source = "" in
    let pos0 = Scanning.init () in
    let token_type1, pos1 =
        Scanning.scan_token source pos0 |> raise_on_error in
    let token_type2, pos2 =
        Scanning.scan_token source pos1 |> raise_on_error in
    Alcotest.(check token_type_testable)
        "EOF type"
        Token.EOF
        token_type1;
    Alcotest.(check token_type_testable)
        "EOF type"
        Token.EOF
        token_type2;
    Alcotest.(check position_testable)
        "Same position" pos1 pos2

let test_multi_token_scan () =
    let source =
        "var v = 1.2;\n\n// Check.\nif (v >= 0) {\n\tprint \"t\";\n}" in
    let open Token in
    let expected_tokens = [
        { token_type=Var; line=0; };
        { token_type=Identifier "v"; line=0; };
        { token_type=Equal; line=0; };
        { token_type=Number 1.2; line=0; };
        { token_type=Semicolon; line=0; };
        { token_type=If; line=3; };
        { token_type=LeftParen; line=3; };
        { token_type=Identifier "v"; line=3; };
        { token_type=GreaterEqual; line=3; };
        { token_type=Number 0.0; line=3; };
        { token_type=RightParen; line=3; };
        { token_type=LeftBrace; line=3; };
        { token_type=Print; line=4; };
        { token_type=String "t"; line=4; };
        { token_type=Semicolon; line=4; };
        { token_type=RightBrace; line=5; };
        { token_type=EOF; line=5; };
    ] in
    let tokens =
        source
        |> Scanning.scan_tokens
        |> Seq.map Util.get_ok
        |> List.of_seq in
    Alcotest.(check (list token_testable))
        "Tokens equal"
        expected_tokens
        tokens

let test_captures_multiple_errors () =
    let source = "(#=@)" in
    let open Token in
    let expected_token_results = [
        Ok { token_type=LeftParen; line=0; };
        Error "Unexpected character.";
        Ok { token_type=Equal; line=0; };
        Error "Unexpected character.";
        Ok { token_type=RightParen; line=0; };
        Ok { token_type=EOF; line=0; };
    ] in
    let token_results = source |> Scanning.scan_tokens |> List.of_seq in
    Alcotest.(check token_result_list_testable)
        "Token results equal"
        expected_token_results
        token_results

let () =
    Alcotest.run "Scanning test suite"
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
