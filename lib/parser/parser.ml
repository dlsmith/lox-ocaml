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

(* TODO(dlsmith): Error handling
    - Use of `List.hd` and `List.tl` to consume a token needs to include
      error handling.
    - Use of exceptions to handle errors should ideally be converted to
      use of `result` type.
 *)

exception Parse_error of string

let uncons tokens =
    List.hd tokens, List.tl tokens

let rec parse_left_assoc_binary_ops ~subparser match_op tokens =
    let expr, tokens = subparser tokens in
    let op = match List.hd tokens with
    | token -> match_op Token.(token.token_type)
    | exception Failure _ -> None
    in
    match op with
    | None -> expr, tokens
    | Some op ->
        let right, tokens =
            parse_left_assoc_binary_ops
                ~subparser
                match_op
                (List.tl tokens)
        in
        Binary (op, expr, right), tokens

(* primary ->
    NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ; *)
let rec parse_primary tokens =
    let token, tokens = uncons tokens in
    match Token.(token.token_type) with
    | Token.False -> Literal False, tokens
    | Token.True -> Literal True, tokens
    | Token.Nil -> Literal Nil, tokens
    | Token.Number num -> Literal (Number num), tokens
    | Token.String str  -> Literal (String str), tokens
    | Token.LeftParen ->
        begin let expr, tokens = parse_expression tokens in
            let token, tokens = uncons tokens in
            match Token.(token.token_type) with
            | Token.RightParen -> Grouping expr, tokens
            | _ -> raise (Parse_error "Expect ')' after expression.")
        end
    | _ -> raise (Parse_error "Expect expression.")

(* unary -> ( "!" | "-" ) unary | primary ; *)
and parse_unary tokens =
    let token = List.hd tokens in
    let op = match Token.(token.token_type) with
    | Token.Bang -> Some LogicalNot
    | Token.Minus -> Some Negate
    | _ -> None
    in
    match op with
    | None -> parse_primary tokens
    | Some op ->
        let right, tokens = parse_unary (List.tl tokens) in
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
