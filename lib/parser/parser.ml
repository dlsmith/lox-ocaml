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

let rec to_sexp expression =
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

type partial_parse = (expression * token_list, string * token_list) result

type expression_parser = token_list -> partial_parse

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

let is_truthy (value: literal) : bool =
    match value with
    | Nil -> false
    | False -> false
    | _ -> true

let is_equal (left : literal) (right: literal) : bool =
    match left, right with
    | Number left, Number right -> left == right
    | String left, String right -> String.equal left right
    | True, True -> true
    | False, False -> true
    | Nil, Nil -> true
    | _ -> false

let of_bool (value : bool) : literal =
    match value with
    | true -> True
    | false -> False

let error message line =
    Error (Printf.sprintf "[line %i] Error: %s" line message)

let rec evaluate_expression = function
    | Literal value -> Ok value
    | Unary (op, subexpr, LineNumber line) ->
        let* value = evaluate_expression subexpr in
        begin match op with
        | Negate ->
            begin match value with
            | Number num -> Ok (Number (-.num))
            | _ -> error "Operand must be a number." line
            end
        | LogicalNot -> Ok (value |> is_truthy |> not |> of_bool)
        end
    | Grouping subexpr -> evaluate_expression subexpr
    | Binary (op, subexpr1, subexpr2, LineNumber line) ->
        let* l = evaluate_expression subexpr1 in
        let* r = evaluate_expression subexpr2 in
        begin match (op, l, r) with
        | Plus, l, r ->
            begin match (l, r) with
            (* String concatenation *)
            | String l, String r -> Ok (String (l ^ r))
            (* Floating point addition *)
            | Number l, Number r -> Ok (Number (l +. r))
            | _ -> error "Operands must be two numbers or two strings." line
            end
        (* Remaining floating point operations *)
        | Minus, Number l, Number r -> Ok (Number (l -. r))
        | Star, Number l, Number r -> Ok (Number (l *. r))
        | Slash, Number l, Number r -> Ok (Number (l /. r))
        (* Comparison operations *)
        | Less, Number l, Number r -> Ok ((l < r) |> of_bool)
        | LessEqual, Number l, Number r -> Ok ((l <= r) |> of_bool)
        | Greater, Number l, Number r -> Ok ((l > r) |> of_bool)
        | GreaterEqual, Number l, Number r -> Ok ((l >= r) |> of_bool)
        (* Boolean equality operations *)
        | EqualEqual, l, r -> Ok ((is_equal l r) |> of_bool)
        | BangEqual, l, r -> Ok ((is_equal l r) |> not |> of_bool)
        | _ -> error "Operands must be numbers." line
        end
