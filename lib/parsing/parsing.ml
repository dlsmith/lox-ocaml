type literal =
    | Number of float
    | String of string
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

let rec to_sexp expression =
    match expression with
    | Literal literal ->
        begin match literal with
            | Number num -> Float.to_string num
            | String str -> str
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

let head = function
    | [] -> None
    | [x] -> Some x
    | x::_ -> Some x

let tail = function
    | [] -> []
    | [_] -> []
    | _::xs -> xs

let uncons = function
    | [] -> None, []
    | [x] -> Some x, []
    | x::xs -> Some x, xs

let get_token_type token =
    Token.(token.token_type)

let (let*) = Result.bind

let rec parse_left_assoc_binary_ops ~subparser match_op tokens =
    let* expr, tokens = subparser tokens in
    let op_line =
        Option.bind (head tokens) (fun token ->
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
                (tail tokens)
        in
        Ok (Binary (op, expr, right, LineNumber line), tokens)

(* primary ->
    NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ; *)
let rec parse_primary tokens =
    let token, tokens = uncons tokens in
    match Option.map get_token_type token with
    | Some Token.False -> Ok (Literal False, tokens)
    | Some Token.True -> Ok (Literal True, tokens)
    | Some Token.Nil -> Ok (Literal Nil, tokens)
    | Some Token.Number num -> Ok (Literal (Number num), tokens)
    | Some Token.String str  -> Ok (Literal (String str), tokens)
    | Some Token.LeftParen ->
        begin
            let* expr, tokens = parse_expression tokens in
            let token, tokens = uncons tokens in
            match Option.map get_token_type token with
            | Some Token.RightParen -> Ok (Grouping expr, tokens)
            | _ -> Error ("Expect ')' after expression.", tokens)
        end
    | _ -> Error ("Expect expression.", tokens)

(* unary -> ( "!" | "-" ) unary | primary ; *)
and parse_unary tokens =
    let op_line =
        Option.bind (head tokens) (fun token ->
            match get_token_type token with
            | Token.Bang -> Some (LogicalNot, token.line)
            | Token.Minus -> Some (Negate, token.line)
            | _ -> None
        )
    in
    match op_line with
    | None -> parse_primary tokens
    | Some (op, line) ->
        let* right, tokens = parse_unary (tail tokens) in
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
    let token, tokens = uncons tokens in
    match Option.map get_token_type token with
    | Some Token.Semicolon -> Ok (create_stmt expr, tokens)
    | _ -> Error ("Expect ';' after value.", tokens)

(* statement -> ( "print" expression | expression ) ";" ; *)
let parse_statement tokens =
    match Option.map get_token_type (head tokens) with
    | Some Token.Print ->
        parse_statement_variant
            (tail tokens)
            (fun expr -> Print expr)
    | _ ->
        parse_statement_variant
            tokens
            (fun expr -> Expression expr)

(* program -> statement* EOF ; *)
let rec parse_program tokens =
    match tokens with
    | [ Token.{ token_type=EOF; _ } ] -> []
    | tokens ->
        let stmt_result, tokens = match parse_statement tokens with
        | Ok (stmt, tokens) -> Ok stmt, tokens
        | Error (message, tokens) -> Error message, tokens
        in
        stmt_result::(parse_program tokens)
