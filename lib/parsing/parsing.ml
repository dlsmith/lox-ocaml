type literal =
    | Number of float
    | String of string
    | Variable of string
    | True
    | False
    | Nil
[@@deriving show, eq]

type unary_op =
    | Negate
    | LogicalNot

type binary_op =
    | EqualEqual
    | BangEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Plus
    | Minus
    | Star
    | Slash

type line_number = LineNumber of int

type expression =
    | Literal of literal
    | Unary of unary_op * expression * line_number
    | Binary of binary_op * expression * expression * line_number
    | Grouping of expression

type statement =
    | Expression of expression
    | Print of expression
    | VariableDeclaration of string * (expression option)

let rec to_sexp expression =
    match expression with
    | Literal literal ->
        begin match literal with
            | Number num -> Float.to_string num
            | String str -> str
            | Variable name -> Printf.sprintf "(var %s)" name
            | True -> "true"
            | False -> "false"
            | Nil -> "nil"
        end
    | Unary (op, subexpr, _) ->
        let op_str = match op with
        | Negate -> "-"
        | LogicalNot -> "!"
        in
        Printf.sprintf "(%s %s)" op_str (to_sexp subexpr)
    | Binary (op, subexpr1, subexpr2, _) ->
        let op_str = match op with
        | EqualEqual -> "=="
        | BangEqual -> "!="
        | Less -> "<"
        | LessEqual -> "<="
        | Greater -> ">"
        | GreaterEqual -> ">="
        | Plus -> "+"
        | Minus -> "-"
        | Star -> "*"
        | Slash -> "/"
        in
        Printf.sprintf
            "(%s %s %s)"
            op_str
            (to_sexp subexpr1)
            (to_sexp subexpr2)
    | Grouping subexpr ->
        Printf.sprintf "(group %s)" (to_sexp subexpr)

exception Parse_error of string

type token_list = Token.token list

let get_token_type token =
    Token.(token.token_type)

let (let*) = Result.bind

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
    let token, rest_tokens = Util.uncons tokens in
    match Option.map get_token_type token with
    | Some Token.False -> Ok (Literal False, rest_tokens)
    | Some Token.True -> Ok (Literal True, rest_tokens)
    | Some Token.Nil -> Ok (Literal Nil, rest_tokens)
    | Some Token.Number num -> Ok (Literal (Number num), rest_tokens)
    | Some Token.String str  -> Ok (Literal (String str), rest_tokens)
    | Some Token.Identifier id -> Ok (Literal (Variable id), rest_tokens)
    | Some Token.LeftParen ->
        let* expr, rest_tokens = parse_expression rest_tokens in
        begin match Option.map get_token_type (Util.head rest_tokens) with
        | Some Token.RightParen -> Ok (Grouping expr, (Util.tail rest_tokens))
        | _ -> Error ("Expect ')' after expression.", rest_tokens)
        end
    | _ -> Error ("Expect expression.", tokens)

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

(* expression -> equality *)
and parse_expression tokens =
    parse_equality tokens

let parse_statement_variant tokens create_stmt =
    let* expr, tokens = parse_expression tokens in
    let token = Util.head tokens in
    match Option.map get_token_type token with
    | Some Token.Semicolon -> Ok (create_stmt expr, Util.tail tokens)
    | _ -> Error ("Expect ';' after value.", tokens)

(* statement -> ( "print" expression | expression ) ";" ; *)
let parse_statement tokens =
    match Option.map get_token_type (Util.head tokens) with
    | Some Token.Print ->
        parse_statement_variant
            (Util.tail tokens)
            (fun expr -> Print expr)
    | _ ->
        parse_statement_variant
            tokens
            (fun expr -> Expression expr)

let consume tokens fn =
    let* token = tokens |> Util.head |> Option.to_result ~none:tokens in
    match fn token with
    | Some v -> Ok (v, Util.tail tokens)
    | None -> Error tokens

let match_type token_type token =
    if get_token_type token == token_type then Some token else None

let match_identifier = function
    | Token.{ token_type=Identifier name; _ } -> Some name
    | _ -> None

(* declaration -> ( "var" IDENTIFIER ( "=" expression )? ) | statement ";" ; *)
let parse_declaration tokens =
    (* TODO(dlsmith): What an unreadable mess. A couple of thoughts:

        - The `Error` case from `consume` is only sometimes an error. We just
          want to carry the remaining tokens, and we can't do that with an
          `Option`, which is a better fit for what's going on. We could return
          a `'a option * Token.token list`, but then we can't pipeline.
        - The type of `consume`'s result also doesn't play well with the result
          we ultimately want to return from this function, so we're limited
          in the extent to which we can use `let *`, which definitely harms
          readability.

        Together, these point to using a custom type in place of Result/Option,
        and using `let*` to allow us to bind in a way that's compatible with
        this type, but will also produce the correct error format in the case
        of early exit.

    *)
    match consume tokens (match_type Token.Var) with
    | Error tokens -> parse_statement tokens
    | Ok (_, tokens) ->
        match consume tokens match_identifier with
        | Ok (var_name, tokens) ->
            let* init_expr, tokens =
                match consume tokens (match_type Token.Equal) with
                | Error tokens -> Ok (None, tokens)
                | Ok (_, tokens) ->
                    let* expr, tokens = parse_expression tokens in
                    Ok (Some expr, tokens)
            in
            begin match consume tokens (match_type Token.Semicolon) with
            | Error tokens ->
                Error ("Expect ';' after variable declaration.", tokens)
            | Ok (_, tokens) ->
                Ok (VariableDeclaration (var_name, init_expr), tokens)
            end
        | Error _ -> Error ("Expect variable name.", tokens)

(* program -> statement* EOF ; *)
let rec parse_program tokens =
    match tokens with
    | [ Token.{ token_type=EOF; _ } ] -> []
    | tokens ->
        let stmt_result, tokens = match parse_declaration tokens with
        | Ok (stmt, tokens) -> Ok stmt, tokens
        | Error (message, tokens) ->
            (* TODO(dlsmith): Synchronize. The book actually synchronizes inside
               `parse_declaration` if an error occurs (via exception), though
               it makes more sense to me that it goes here. *)
            Error message, tokens
        in
        stmt_result :: (parse_program tokens)
