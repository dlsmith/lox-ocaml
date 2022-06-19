open Parsing

let (let*) = Result.bind

module Env = struct
    type t = {
        (* TODO(dlsmith): Use more efficient data structure. *)
        (* TODO(dlsmith): Can we do this without mutability (i.e., refs)? *)
        values: (string * literal) list ref;
        parent: t ref option
    }

    let make ~parent = {values=ref []; parent=parent}

    let contains t key =
        List.mem_assoc key !(t.values)

    let get_parent env = env.parent

    let define env key value =
        let base_values = if contains env key
            then List.remove_assoc key !(env.values)
            else !(env.values) in
        env.values := (key, value) :: base_values;
        env

    let rec set env key value =
        (* TODO(dlsmith): Lots of unnecessary `contains` checks due to use of
           `List`. *)
        if contains env key
            then Ok (define env key value)
            else match env.parent with
            | Some parent_ref ->
                let* updated_parent = set !parent_ref key value in
                parent_ref := updated_parent;
                Ok env
            | None -> Error ("Undefined variable '" ^ key ^ "'.")

    let rec get env key =
        match List.assoc_opt key !(env.values), env.parent with
        | Some v, _ -> Ok v
        | None, Some env_ref -> get !env_ref key
        | None, None -> Error ("Undefined variable '" ^ key ^ "'.")
end

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

let get_line_number (expr : expression) : line_number =
    match expr with
    | Literal (_, line_number)
    | Unary (_, _, line_number)
    | Binary (_, _, _, line_number)
    | Grouping (_, line_number)
    | Assignment (_, _, line_number) -> line_number

let error message line =
    Error (Printf.sprintf "[line %i] Error: %s" line message)

let rec evaluate_expression env = function
    | Literal (Variable name, LineNumber line) ->
        begin match Env.get env name with
        | Ok value -> Ok (value, env)
        | Error message -> error message line
        end
    | Literal (value, _) -> Ok (value, env)
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
    | Grouping (subexpr, _) -> evaluate_expression env subexpr
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
        let* value, env = evaluate_expression env expr in
        match Env.set env name value with
        | Ok env -> Ok (value, env)
        | Error message -> error message line

let rec evaluate_statement env stmt =
    match stmt with
    | Expression expr ->
        let* value, env = evaluate_expression env expr in
        Ok (Some value, env)
    | Print expr ->
        let* value, env = evaluate_expression env expr in
        let line_number = get_line_number expr in
        Literal (value, line_number) |> to_sexp |> print_endline;
        Ok (None, env)
    | Block stmts ->
        let parent_env = env in
        let env = Env.make ~parent:(Some (ref parent_env)) in
        (* For a value to be returned from the program, it must be at the
           top-level of the program, not within a block. We may want to revisit
           this later.*)
        let* _ = evaluate_statements env stmts in
        Ok (None, parent_env)
    | VariableDeclaration (name, init_expr) ->
        let* value, env = match init_expr with
        | Some expr -> evaluate_expression env expr
        | None -> Ok (Parsing.Nil, env)
        in
        Ok (None, Env.define env name value)

and evaluate_statements env stmts =
    match stmts with
    (* In practice, this base case won't be executed unless this function is
       called explicitly with an empty list of statements *)
    | [] -> Ok None
    | stmt :: stmts ->
        let* output, env = evaluate_statement env stmt in
        (* Return the output if this is the last statement. This enables our
           programs to produce values rather than purely executing for side
           effects.

           This is an intentional diversion from the book, the primary
           motivation being to simplify testing. May revisit later. *)
        match stmts with [] -> Ok output | _ -> evaluate_statements env stmts

let evaluate_program = evaluate_statements
