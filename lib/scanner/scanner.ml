(* TODO(dlsmith): Add line context when returning `Error`s*)

let (let*) = Result.bind

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
    (* TODO(dlsmith): Catch Invalid_argument exception and return `result` *)
    String.sub s start_index len

let parse_string_literal source pos =
    let str = substring source (pos.start + 1) (pos.current - 1) in
    Ok str

let parse_number_literal source pos =
    let number_str = substring source pos.start pos.current in
    match Float.of_string number_str with
    | exception Failure _ -> Error "Failed to parse number."
    | number -> Ok number

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
let create_token source pos token_type =
    Ok Token.{
        token_type=token_type;
        lexeme=substring source pos.start pos.current;
        line=pos.line
    }

let rec ignore_line source pos =
    let c, pos = advance source pos in
    match c with
    | Some '\n' -> pos |> inc_line |> update_start
    | None -> update_start pos
    | _ -> ignore_line source pos

let rec scan_string_token source pos =
    match peek source pos with
    | None -> Error ("Unterminated string.", pos)
    | Some '"' -> Ok (pos |> inc_current)
    | Some '\n' -> scan_string_token source (pos |> inc_current |> inc_line)
    | _ -> scan_string_token source (pos |> inc_current)

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
    | Some '.' -> scan_digit_string source (pos |> inc_current)
    | _ -> pos

let rec scan_identifier_token source pos =
    match peek source pos with
    | Some c when is_alpha_numeric c ->
        scan_identifier_token source (inc_current pos)
    | _ -> pos

let match_next source pos c then_type else_type =
    match peek source pos with
    | Some test_c when test_c = c -> then_type, inc_current pos
    | _ -> else_type, pos

let result_attach b r =
    match r with
    | Ok a -> Ok (a, b)
    | Error e -> Error (e, b)

let rec scan_token source pos =
    let c, pos = advance source pos in
    match c with
    | None -> Ok (Token.EOF, pos)
    | Some c -> consume_char c source pos

and consume_char c source pos =
    match c with
    (* Whitespace *)
    | ' '  | '\r' | '\t' -> scan_token source (pos |> update_start)
    | '\n' -> scan_token source (pos |> inc_line |> update_start)

    (* Single character tokens *)
    | '(' -> Ok (Token.LeftParen, pos)
    | ')' -> Ok (Token.RightParen, pos)
    | '{' -> Ok (Token.LeftBrace, pos)
    | '}' -> Ok (Token.RightBrace, pos)
    | ',' -> Ok (Token.Comma, pos)
    | '.' -> Ok (Token.Dot, pos)
    | '-' -> Ok (Token.Minus, pos)
    | '+' -> Ok (Token.Plus, pos)
    | ';' -> Ok (Token.Semicolon, pos)
    | '*' -> Ok (Token.Star, pos)

    (* Two character operators *)
    | '!' -> Ok (match_next source pos '=' Token.BangEqual Token.Bang)
    | '=' -> Ok (match_next source pos '=' Token.EqualEqual Token.Equal)
    | '<' -> Ok (match_next source pos '=' Token.LessEqual Token.Less)
    | '>' -> Ok (match_next source pos '=' Token.GreaterEqual Token.Greater)

    (* Comment *)
    | '/' ->
        begin match peek source pos with
        (* If comment, resume scanning after end of line *)
        | Some '/' -> scan_token source (ignore_line source pos)
        | _ -> Ok (Token.Slash, pos)
        end

    (* String literal *)
    | '"' ->
        let* pos = scan_string_token source pos in
        parse_string_literal source pos
        |> Result.map (fun str -> Token.String str)
        |> result_attach pos

    (* Number literal *)
    | c when is_digit c -> 
        let pos = scan_number_token source pos in
        parse_number_literal source pos
        |> Result.map (fun num -> Token.Number num)
        |> result_attach pos

    (* Identifier *)
    | c when is_alpha c ->
        let pos = scan_identifier_token source pos in
        substring source pos.start pos.current
        |> Token.as_keyword
        |> Option.value ~default:Token.Identifier
        |> Result.ok
        |> result_attach pos

    (* Failure *)
    | _ -> Error ("Unexpected character.", pos)

let scan_and_extract_token source pos =
    let token_type_result, pos =
        match scan_token source pos with
        | Ok (token_type, pos) -> Ok token_type, pos
        | Error (message, pos) -> Error message, pos
    in
    token_type_result
    |> Result.map (create_token source pos)
    |> Result.join
    |> result_attach (update_start pos)

let rec scan_tokens source ?(pos=init()) =
    match scan_and_extract_token source pos with
    | Ok (token, pos) ->
        begin match token.token_type with
        | Token.EOF -> Seq.return (Ok token)
        | _ -> Seq.cons (Ok token) (scan_tokens source ~pos)
        end
    | Error (message, pos) ->
        Seq.cons (Error message) (scan_tokens source ~pos)
