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

(* TODO(dlsmith): Change use of exceptions to Result *)

exception Parse_error of string

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

let rec parse_left_assoc_binary_ops ~subparser match_op tokens =
    let expr, tokens = subparser tokens in
    let op = match head tokens with
    | None -> None
    | Some token -> token |> get_token_type |> match_op
    in
    match op with
    | None -> expr, tokens
    | Some op ->
        let right, tokens =
            parse_left_assoc_binary_ops
                ~subparser
                match_op
                (tail tokens)
        in
        Binary (op, expr, right), tokens

(* primary ->
    NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ; *)
let rec parse_primary tokens =
    let token, tokens = uncons tokens in
    match Option.map get_token_type token with
    | Some Token.False -> Literal False, tokens
    | Some Token.True -> Literal True, tokens
    | Some Token.Nil -> Literal Nil, tokens
    | Some Token.Number num -> Literal (Number num), tokens
    | Some Token.String str  -> Literal (String str), tokens
    | Some Token.LeftParen ->
        begin let expr, tokens = parse_expression tokens in
            let token, tokens = uncons tokens in
            match Option.map get_token_type token with
            | Some Token.RightParen -> Grouping expr, tokens
            | _ -> raise (Parse_error "Expect ')' after expression.")
        end
    | _ -> raise (Parse_error "Expect expression.")

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
        let right, tokens = parse_unary (tail tokens) in
        Unary (op, right), tokens

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
