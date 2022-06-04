type literal =
    | Number of float
    | String of string
    | True
    | False
    | Nil

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

type expression =
    | Literal of literal
    | Unary of unary_op * expression
    | Binary of binary_op * expression * expression
    | Grouping of expression

let rec print_ast expression =
    match expression with
    | Literal literal ->
        begin
            match literal with
            | Number num -> Float.to_string num
            | String str -> str
            | True -> "true"
            | False -> "false"
            | Nil -> "nil"
        end
    | Unary (op, subexpr) ->
        let op_str = match op with
        | Negate -> "-"
        | LogicalNot -> "!"
        in
        Printf.sprintf "(%s %s)" op_str (print_ast subexpr)
    | Binary (op, subexpr1, subexpr2) ->
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
            (print_ast subexpr1)
            (print_ast subexpr2)
    | Grouping subexpr ->
        Printf.sprintf "(group %s)" (print_ast subexpr)

exception Parse_error of string

type token_list = Token.token list

type partial_parse = (expression * token_list, string * token_list) result

type expression_parser = token_list -> partial_parse

let head l =
    match l with
    | [] -> None
    | [x] -> Some x
    | x::_ -> Some x

let tail l =
    match l with
    | [] -> []
    | [_] -> []
    | _::xs -> xs

let uncons l =
    match l with
    | [] -> None, []
    | [x] -> Some x, []
    | x::xs -> Some x, xs

let get_token_type token =
    Token.(token.token_type)

let (let*) = Result.bind

let rec parse_left_assoc_binary_ops ~subparser match_op tokens =
    let* expr, tokens = subparser tokens in
    let op = Option.bind (head tokens) (fun token ->
        token |> get_token_type |> match_op) in
    match op with
    | None -> Ok (expr, tokens)
    | Some op ->
        let* right, tokens =
            parse_left_assoc_binary_ops
                ~subparser
                match_op
                (tail tokens)
        in
        Ok (Binary (op, expr, right), tokens)

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
    let token = head tokens in
    let op = match Option.map get_token_type token with
    | Some Token.Bang -> Some LogicalNot
    | Some Token.Minus -> Some Negate
    | _ -> None
    in
    match op with
    | None -> parse_primary tokens
    | Some op ->
        let* right, tokens = parse_unary (tail tokens) in
        Ok (Unary (op, right), tokens)

(* factor -> unary ( ( "/" | "*" ) unary )* ; *)
and parse_factor tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_unary
        begin fun token_type ->
            match token_type with
            | Token.Slash -> Some Slash
            | Token.Star -> Some Star
            | _ -> None
        end
        tokens

(* term -> factor ( ( "-" | "+" ) factor )* ; *)
and parse_term tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_factor
        begin fun token_type ->
            match token_type with
            | Token.Minus -> Some Minus
            | Token.Plus -> Some Plus
            | _ -> None
        end
        tokens

(* comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ; *)
and parse_comparison tokens =
    parse_left_assoc_binary_ops
        ~subparser:parse_term
        begin fun token_type ->
            match token_type with
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
        begin fun token_type ->
            match token_type with
            | Token.BangEqual -> Some BangEqual
            | Token.EqualEqual -> Some EqualEqual
            | _ -> None
        end
        tokens

(* expression -> equality *)
and parse_expression tokens =
    parse_equality tokens
