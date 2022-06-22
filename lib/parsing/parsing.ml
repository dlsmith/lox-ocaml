open Ast

exception Parse_error of string

type token_list = Token.token list

let get_token_type token =
    Token.(token.token_type)

let (let*) = Result.bind

(* Consume a single token if it passes the given predicate. *)
let consume tokens pred =
    match Option.bind (Util.head tokens) pred with
    | Some v -> Some v, (Util.tail tokens)
    | None -> (None, tokens)

let match_type token_type token =
    if get_token_type token == token_type then Some token else None

let match_identifier = function
    | Token.{ token_type=Identifier name; _ } -> Some name
    | _ -> None

let rec parse_left_assoc_binary_ops ~subparser match_op tokens =
    let* expr, tokens = subparser tokens in
    let op_line =
        Option.bind (Util.head tokens) (fun token ->
            token
            |> get_token_type
            |> match_op
            |> Option.map (fun op -> op, Token.(token.line))
        )
    in
    match op_line with
    | None -> Ok (expr, tokens)
    | Some (op, line) ->
        let* right, tokens =
            parse_left_assoc_binary_ops
                ~subparser
                match_op
                (Util.tail tokens)
        in
        Ok (Binary (op, expr, right, LineNumber line), tokens)

(* primary ->
    NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ; *)
let rec parse_primary tokens =
    let default_error = ("Expect expression.", tokens) in
    let* token = tokens |> Util.head |> Option.to_result ~none:default_error in
    let line_number = LineNumber Token.(token.line) in
    let tokens = Util.tail tokens in
    match get_token_type token with
    | Token.False -> Ok (Literal (False, line_number), tokens)
    | Token.True -> Ok (Literal (True, line_number), tokens)
    | Token.Nil -> Ok (Literal (Nil, line_number), tokens)
    | Token.Number num -> Ok (Literal (Number num, line_number), tokens)
    | Token.String str  -> Ok (Literal (String str, line_number), tokens)
    | Token.Identifier id -> Ok (Literal (Variable id, line_number), tokens)
    | Token.LeftParen ->
        let* expr, tokens = parse_expression tokens in
        begin match Option.map get_token_type (Util.head tokens) with
        | Some Token.RightParen ->
            Ok (Grouping (expr, line_number), (Util.tail tokens))
        | _ -> Error ("Expect ')' after expression.", tokens)
        end
    | _ -> Error default_error

(* unary -> ( "!" | "-" ) unary | primary ; *)
and parse_unary tokens =
    let op_line =
        Option.bind (Util.head tokens) (fun token ->
            match get_token_type token with
            | Token.Bang -> Some (LogicalNot, token.line)
            | Token.Minus -> Some (Negate, token.line)
            | _ -> None
        )
    in
    match op_line with
    | None -> parse_primary tokens
    | Some (op, line) ->
        let* right, tokens = parse_unary (Util.tail tokens) in
        Ok (Unary (op, right, LineNumber line), tokens)

(* factor -> unary ( ( "/" | "*" ) unary )* ; *)
and parse_factor tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_unary
        begin function
            | Token.Slash -> Some Slash
            | Token.Star -> Some Star
            | _ -> None
        end
        tokens

(* term -> factor ( ( "-" | "+" ) factor )* ; *)
and parse_term tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_factor
        begin function
            | Token.Minus -> Some Minus
            | Token.Plus -> Some Plus
            | _ -> None
        end
        tokens

(* comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ; *)
and parse_comparison tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_term
        begin function
            | Token.Greater -> Some Greater
            | Token.GreaterEqual -> Some GreaterEqual
            | Token.Less -> Some Less
            | Token.LessEqual -> Some LessEqual
            | _ -> None
        end
        tokens

(* equality -> comparison ( ( "!=" | "==" ) comparison )* ; *)
and parse_equality tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_comparison
        begin function
            | Token.BangEqual -> Some BangEqual
            | Token.EqualEqual -> Some EqualEqual
            | _ -> None
        end
        tokens

(* assignment -> IDENTIFIER "=" assignment | equality *)
and parse_assignment tokens =
    let* expr, tokens = parse_equality tokens in
    match consume tokens (match_type Token.Equal) with
    | Some equal_token, tokens ->
        (* Now we know we're parsing an assignment, so we should check the
           previously parsed expression to make sure it's a valid l-value. *)
        begin match expr with
        | Literal (Variable name, _) ->
            let* value, tokens = parse_assignment tokens in
            let line_number = LineNumber equal_token.line in
            Ok (Assignment (name, value, line_number), tokens)
        | _ -> Error ("Invalid assignment target.", tokens)
        end
    | None, tokens -> Ok (expr, tokens)

(* expression -> assignment *)
and parse_expression tokens =
    parse_assignment tokens

let parse_statement_variant tokens create_stmt =
    let* expr, tokens = parse_expression tokens in
    match consume tokens (match_type Token.Semicolon) with
    | Some _, tokens -> Ok (create_stmt expr, tokens)
    | None, tokens -> Error ("Expect ';' after value.", tokens)

(* statement -> ( "print" expression | block | expression ) ";" ; *)
let rec parse_statement tokens =
    match Option.map get_token_type (Util.head tokens) with
    | Some Token.Print ->
        parse_statement_variant
            (Util.tail tokens)
            (fun expr -> Print expr)
    (* block -> "{" declaration* "}" ; *)
    | Some Token.LeftBrace ->
        let rec parse_block tokens =
            match Option.map get_token_type (Util.head tokens) with
            | None | Some Token.EOF | Some Token.RightBrace ->
                Ok ([], tokens)
            | _ ->
                let* stmt, tokens = parse_declaration tokens in
                let* stmts, tokens = parse_block tokens in
                Ok (stmt :: stmts, tokens)
        in

        let* stmts, tokens = parse_block (Util.tail tokens) in
        begin match consume tokens (match_type Token.RightBrace) with
        | Some _, tokens -> Ok (Block stmts, tokens)
        | None, tokens -> Error ("Expect '}' after block.", tokens)
        end
    | _ ->
        parse_statement_variant
            tokens
            (fun expr -> Expression expr)

(* declaration -> ( "var" IDENTIFIER ( "=" expression )? ) | statement ";" ; *)
and parse_declaration tokens =
    match consume tokens (match_type Token.Var) with
    | Some _, tokens ->
        begin match consume tokens match_identifier with
        | Some var_name, tokens ->
            (* Parse the initializer expression if given *)
            let* init_expr, tokens =
                match consume tokens (match_type Token.Equal) with
                | None, tokens -> Ok (None, tokens)
                | Some _, tokens ->
                    let* expr, tokens = parse_expression tokens in
                    Ok (Some expr, tokens)
            in
            (* Make sure we finish with a semicolon before returning *)
            begin match consume tokens (match_type Token.Semicolon) with
            | None, tokens ->
                Error ("Expect ';' after variable declaration.", tokens)
            | Some _, tokens ->
                Ok (VariableDeclaration (var_name, init_expr), tokens)
            end
        | None, tokens -> Error ("Expect variable name.", tokens)
        end
    | None, tokens -> parse_statement tokens

let rec synchronize tokens =
    match Option.map get_token_type (Util.head tokens) with
    (* We've run out of tokens *)
    | None
    | Some Token.EOF
    (* We're about to start a new statement *)
    | Some Token.Class
    | Some Token.Fun
    | Some Token.Var
    | Some Token.For
    | Some Token.If
    | Some Token.While
    | Some Token.Print
    | Some Token.Return -> tokens
    (* We've finished a statement. *)
    | Some Token.Semicolon -> (Util.tail tokens)
    (* Otherwise, continue *)
    | _ -> synchronize (Util.tail tokens)

(* program -> declaration* EOF ; *)
let rec parse_program tokens =
    match tokens with
    | [ Token.{ token_type=EOF; _ } ] -> []
    | tokens ->
        let stmt_result, tokens = match parse_declaration tokens with
        | Ok (stmt, tokens) -> Ok stmt, tokens
        | Error (message, tokens) -> Error message, (synchronize tokens)
        in
        stmt_result :: (parse_program tokens)
