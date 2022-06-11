open Parsing

let (let*) = Result.bind

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

let evaluate_statement = function
    | Expression expr ->
        evaluate_expression expr |> Result.map (fun _ -> ())
    | Print expr ->
        let* value = evaluate_expression expr in
        Literal value |> to_sexp |> print_endline;
        Ok ()

let rec evaluate_program stmts =
    let stmt, stmts = Util.uncons stmts in
    match stmt with
    | None -> Ok ()
    | Some s ->
        let* _ = evaluate_statement s in
        evaluate_program stmts
