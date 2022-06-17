open Parsing

module Env = struct
    (* TODO(dlsmith): Use more efficient data structure. *)
    type t = (string * literal) list
    let make () = []
    let contains t key =
        List.mem_assoc key t
    let set env key value =
        let env = if contains env key
            then List.remove_assoc key env
            else env in
        (key, value) :: env
    let get env key =
        match List.assoc_opt key env with
        | Some v -> Ok v
        | None -> Error (Printf.sprintf "Undefined variable '%s'." key)
end

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

let rec evaluate_expression env = function
    | Literal (Variable name) ->
        let* value = Env.get env name in
        Ok (value, env)
    | Literal value -> Ok (value, env)
    | Unary (op, subexpr, LineNumber line) ->
        let* value, env = evaluate_expression env subexpr in
        begin match op with
        | Negate ->
            begin match value with
            | Number num -> Ok (Number (-.num), env)
            | _ -> error "Operand must be a number." line
            end
        | LogicalNot -> Ok (value |> is_truthy |> not |> of_bool, env)
        end
    | Grouping subexpr -> evaluate_expression env subexpr
    | Binary (op, subexpr1, subexpr2, LineNumber line) ->
        let* l, env = evaluate_expression env subexpr1 in
        let* r, env = evaluate_expression env subexpr2 in
        begin match (op, l, r) with
        | Plus, l, r ->
            begin match (l, r) with
            (* String concatenation *)
            | String l, String r -> Ok (String (l ^ r), env)
            (* Floating point addition *)
            | Number l, Number r -> Ok (Number (l +. r), env)
            | _ -> error "Operands must be two numbers or two strings." line
            end
        (* Remaining floating point operations *)
        | Minus, Number l, Number r -> Ok (Number (l -. r), env)
        | Star, Number l, Number r -> Ok (Number (l *. r), env)
        | Slash, Number l, Number r -> Ok (Number (l /. r), env)
        (* Comparison operations *)
        | Less, Number l, Number r -> Ok ((l < r) |> of_bool, env)
        | LessEqual, Number l, Number r -> Ok ((l <= r) |> of_bool, env)
        | Greater, Number l, Number r -> Ok ((l > r) |> of_bool, env)
        | GreaterEqual, Number l, Number r -> Ok ((l >= r) |> of_bool, env)
        (* Boolean equality operations *)
        | EqualEqual, l, r -> Ok ((is_equal l r) |> of_bool, env)
        | BangEqual, l, r -> Ok ((is_equal l r) |> not |> of_bool, env)
        | _ -> error "Operands must be numbers." line
        end
    | Assignment (name, expr, LineNumber line) ->
        if Env.contains env name
            then
                let* value, env = evaluate_expression env expr in
                let env = Env.set env name value in
                Ok (value, env)
            else error ("Undefined variable '" ^ name ^ "'.") line

let evaluate_statement env stmt =
    match stmt with
    | Expression expr ->
        let* value, env = evaluate_expression env expr in
        Ok (Some value, env)
    | Print expr ->
        let* value, env = evaluate_expression env expr in
        Literal value |> to_sexp |> print_endline;
        Ok (None, env)
    | VariableDeclaration (name, init_expr) ->
        let* value, env = match init_expr with
        | Some expr -> evaluate_expression env expr
        | None -> Ok (Parsing.Nil, env)
        in
        Ok (None, Env.set env name value)

let rec evaluate_program env stmts =
    match stmts with
    (* In practice, this base case won't be executed unless the top-level call
       to `execute_program` passes an empty list of statements *)
    | [] -> Ok None
    | stmt :: stmts ->
        let* output, env = evaluate_statement env stmt in
        (* Return the output if this is the last statement. This enables our
           programs to produce values rather than purely executing for side
           effects.

           This is an intentional diversion from the book, the primary
           motivation being to simplify testing. May revisit later. *)
        match stmts with [] -> Ok output | _ -> evaluate_program env stmts
