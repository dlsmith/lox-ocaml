open Ast

let (let*) = Result.bind

exception Parse_error of string

type token_list = Token.token list

let get_token_type token = Token.(token.token_type)

let peek tokens = Option.map get_token_type (Util.head tokens)

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

let consume_or_error tokens token_type message =
    match consume tokens (match_type token_type) with
    | Some _, tokens -> Ok tokens
    | None, tokens -> Error (message, tokens)

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
        begin match peek tokens with
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

(* logic_or -> equality ( "and" equality )* ; *)
and parse_logical_and tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_equality
        begin function
            | Token.And -> Some And
            | _ -> None
        end
        tokens

(* logic_or -> logic_and ( "or" logic_and )* ; *)
and parse_logical_or tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_logical_and
        begin function
            | Token.Or -> Some Or
            | _ -> None
        end
        tokens

(* assignment -> IDENTIFIER "=" assignment | logic_or *)
and parse_assignment tokens =
    let* expr, tokens = parse_logical_or tokens in
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

(* statement -> ( "print" expression | block | expression ) ";" ; *)
let rec parse_statement tokens =
    match peek tokens with
    | Some Token.Print ->
        let* expr, tokens = parse_expression (Util.tail tokens) in
        let* tokens =
            consume_or_error
                tokens
                Token.Semicolon
                "Expect ';' after value."
        in
        Ok (Print expr, tokens)
    | Some Token.If ->
        let tokens = Util.tail tokens in
        let* tokens =
            consume_or_error
                tokens
                Token.LeftParen
                "Expect '(' after 'if'."
        in
        let* condition, tokens = parse_expression tokens in
        let* tokens =
            consume_or_error
                tokens
                Token.RightParen
                "Expect ')' after if condition."
        in
        let* then_branch, tokens = parse_statement tokens in
        begin match consume tokens (match_type Token.Else) with
        | Some _, tokens ->
            let* else_branch, tokens = parse_statement tokens in
            Ok (If (condition, then_branch, Some else_branch), tokens)
        | None, tokens ->
            Ok (If (condition, then_branch, None), tokens)
        end
    | Some Token.While ->
        let tokens = Util.tail tokens in
        let* tokens =
            consume_or_error
                tokens
                Token.LeftParen
                "Expect '(' after 'while'."
        in
        let* condition, tokens = parse_expression tokens in
        let* tokens =
            consume_or_error
                tokens
                Token.RightParen
                "Expect ')' after if condition."
        in
        let* body, tokens = parse_statement tokens in
        Ok (While (condition, body), tokens)
    (* block -> "{" declaration* "}" ; *)
    | Some Token.LeftBrace ->
        let rec parse_block tokens =
            match peek tokens with
            | None | Some Token.EOF | Some Token.RightBrace ->
                Ok ([], tokens)
            | _ ->
                let* stmt, tokens = parse_declaration tokens in
                let* stmts, tokens = parse_block tokens in
                Ok (stmt :: stmts, tokens)
        in

        let* stmts, tokens = parse_block (Util.tail tokens) in
        let* tokens =
            consume_or_error
                tokens
                Token.RightBrace
                "Expect '}' after block."
        in
        Ok (Block stmts, tokens)
    | _ ->
        let* expr, tokens = parse_expression tokens in
        let* tokens =
            consume_or_error
                tokens
                Token.Semicolon
                "Expect ';' after value."
        in
        Ok (Expression expr, tokens)

(* declaration -> ( "var" IDENTIFIER ( "=" expression )? ) | statement ";" ; *)
and parse_declaration tokens =
    match consume tokens (match_type Token.Var) with
    | Some _, tokens ->
        begin match consume tokens match_identifier with
        | Some var_name, tokens ->
            (* Parse the initializer expression if given *)
            let* init_expr, tokens =
                match consume tokens (match_type Token.Equal) with
                | Some _, tokens ->
                    let* expr, tokens = parse_expression tokens in
                    Ok (Some expr, tokens)
                | None, tokens -> Ok (None, tokens)
            in
            (* Make sure we finish with a semicolon before returning *)
            let* tokens =
                consume_or_error
                    tokens
                    Token.Semicolon
                    "Expect ';' after variable declaration."
            in
            Ok (VariableDeclaration (var_name, init_expr), tokens)
        | None, tokens -> Error ("Expect variable name.", tokens)
        end
    | None, tokens -> parse_statement tokens

let rec synchronize tokens =
    match peek tokens with
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
