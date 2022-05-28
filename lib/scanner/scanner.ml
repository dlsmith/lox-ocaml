type position = {
    start: int;
    current: int;
    line: int;
} [@@deriving show, eq]

let init () = { start = 0; current = 0; line = 0 }

let inc_current pos =
    {pos with current = pos.current + 1}

let inc_line pos =
    { pos with line = pos.line + 1 }

let update_start pos =
    { pos with start = pos.current }

let index_out_of_bounds str index =
    index < 0 || index >= String.length str 

let peek source pos =
    if index_out_of_bounds source pos.current then
        None
    else
        Some source.[pos.current]

let advance source pos =
    match peek source pos with
    | Some c -> Some c, inc_current pos
    | None -> None, pos

let substring s start_index end_index =
    let len = end_index - start_index in
    String.sub s start_index len

let parse_string_literal source pos =
    let str = substring source (pos.start + 1) (pos.current - 1) in
    (* TODO(dlsmith): Handle index error *)
    Ok (Literal.String str)

let parse_number_literal source pos =
    let number_str = substring source pos.start pos.current in
    match Float.of_string number_str with
    | exception Failure _ -> Error "Failed to parse number."
    | number -> Ok (Literal.Number number)

(* TODO(dlsmith): There's something less than ideal here in that you can
 pass arguments that are inconsistent. E.g., pass a one-character token type
 with a multi-character `position`. Maybe it is better to do the extraction
 immediately when the type is identified. (This is how things started, but
 the code was messier.)

 The reason I ended up abandoning this was to enable the recursion for
 handling whitespace. When working with positions, we can recurse over
 characters to ignore them until we end up at a token. Without this, we'd
 need to sometimes return nothing, and let the caller handle the skipping
 logic. This isn't a huge burden, but it's a nice API that says, "just give
 me the next token and tell me where you left off."
*)
let extract_token source pos token_type = 
    (* TODO(dlsmith): Handle error cases, OOB, etc. *)
    let base_token = Token.{
        token_type=token_type;
        lexeme=substring source pos.start pos.current;
        literal=None;
        line=pos.line
    } in
    let attach_literal literal_result =
        match literal_result with
        | Ok literal -> Ok Token.{base_token with literal=Some literal}
        | Error message -> Error message
    in
    match token_type with
    | Token.String -> attach_literal (parse_string_literal source pos)
    | Token.Number -> attach_literal (parse_number_literal source pos)
    | _ -> Ok base_token

let rec ignore_line source pos =
    let c, pos = advance source pos in
    match c with
    | Some '\n' -> pos |> inc_line |> update_start
    | None -> update_start pos
    | _ -> ignore_line source pos

type scan_string_result =
    | ValidString of position
    | InvalidString of position * string

let rec scan_string_token source pos =
    match peek source pos with
    | None -> InvalidString (pos, "Unterminated string.")
    | Some '"' -> ValidString (inc_current pos)
    | Some '\n' -> scan_string_token source (pos |> inc_current |> inc_line)
    | _ -> scan_string_token source (inc_current pos)

let is_digit c =
    let c_code = Char.code c in
    c_code >= Char.code '0' && c_code <= Char.code '9'

let is_alpha c =
    let c_code = Char.code c in
    (c_code >= Char.code 'a' && c_code <= Char.code 'z') ||
    (c_code >= Char.code 'A' && c_code <= Char.code 'Z') ||
    (c_code == Char.code '_')

let is_alpha_numeric c =
    is_alpha c || is_digit c

let rec scan_digit_string source pos =
    match peek source pos with
    | Some c when is_digit c -> scan_digit_string source (inc_current pos)
    | _ -> pos

let scan_number_token source pos =
    let pos = scan_digit_string source pos in
    match peek source pos with
    (* We're diverging from the spec here by allowing `12.` for `12.0`. We may
       need to revise this later. *)
    | Some '.' -> scan_digit_string source (inc_current pos)
    | _ -> pos

let rec scan_identifier_token source pos =
    match peek source pos with
    | Some c when is_alpha_numeric c ->
        scan_identifier_token source (inc_current pos)
    | _ -> pos

let match_next source pos c then_type else_type =
    match peek source pos with
    | Some test_c when test_c = c -> Ok then_type, (inc_current pos)
    | _ -> Ok else_type, pos

let rec scan_token source pos =
    let c, pos = advance source pos in
    match c with
    (* No more characters to process *)
    | None -> Ok Token.EOF, pos
    | Some c ->
        (* TODO(dlsmith): Best practices for "early return"? *)
        match c with
        (* Whitespace *)
        | ' '  | '\r' | '\t' -> scan_token source (update_start pos)
        | '\n' -> scan_token source (pos |> inc_line |> update_start)

        (* Single character tokens *)
        | '(' -> Ok Token.LeftParen, pos
        | ')' -> Ok Token.RightParen, pos
        | '{' -> Ok Token.LeftBrace, pos
        | '}' -> Ok Token.RightBrace, pos
        | ',' -> Ok Token.Comma, pos
        | '.' -> Ok Token.Dot, pos
        | '-' -> Ok Token.Minus, pos
        | '+' -> Ok Token.Plus, pos
        | ';' -> Ok Token.Semicolon, pos
        | '*' -> Ok Token.Star, pos

        (* Two character operators *)
        (* TODO(dlsmith): It's a little weird we're wrapping as a result in
        some cases but not in others (i.e., here), where it's handled by a
        helper. *)
        | '!' -> match_next source pos '=' Token.BangEqual Token.Bang
        | '=' -> match_next source pos '=' Token.EqualEqual Token.Equal
        | '<' -> match_next source pos '=' Token.LessEqual Token.Less
        | '>' -> match_next source pos '=' Token.GreaterEqual Token.Greater

        (* Comment *)
        | '/' ->
            begin match peek source pos with
            (* If comment, resume scanning after end of line *)
            | Some '/' -> scan_token source (ignore_line source pos)
            | _ -> Ok Token.Slash, pos
            end

        (* String literal *)
        | '"' ->
            begin match scan_string_token source pos with
            | ValidString pos -> Ok Token.String, pos
            | InvalidString (pos, message) -> Error message, pos
            end

        (* Number literal *)
        | c when is_digit c -> Ok Token.Number, (scan_number_token source pos)

        (* Identifier *)
        | c when is_alpha c ->
            let pos = scan_identifier_token source pos in
            let lexeme = substring source pos.start pos.current in
            let token_type =
                match Token.as_keyword lexeme with
                | Some keyword_type -> keyword_type
                | None -> Token.Identifier
            in
            Ok token_type, pos

        (* Failure *)
        | _ -> Error "Unexpected character.", pos

let scan_and_extract_token source pos =
    let token_type_result, pos = scan_token source pos in
    let token_result =
        match token_type_result with
        | Ok token_type -> extract_token source pos token_type
        | Error message -> Error message
    in
    token_result, update_start pos

let rec scan_tokens source ?(pos=init()) =
    let token_result, pos = scan_and_extract_token source pos in
    match token_result with
    | Ok { token_type=Token.EOF; _ } ->
        Seq.return token_result
    | _ ->
        Seq.cons token_result (scan_tokens source ~pos)
