open Ast

(** An internal type extending `Result` to enable short-circuiting via
    `return` statement. We'll use `let*` for `bind` within functions either for
    `Result` or `EvaluationResult`, which accomplishes the short-circuiting. *)
module EvaluationResult = struct
    type 'a evaluation_result =
        | Ok of 'a
        | ReturnValue of Ast.literal
        | Error of string

    let bind r f =
        match r with
        | Ok v -> f v
        | ReturnValue _ as v -> v
        | Error _ as e -> e

    let of_result = function
        | Result.Ok v -> Ok v
        | Result.Error v -> Error v
end

module ER = EvaluationResult

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

(* Modified version of `List.fold_left_map` to support `Result`. *)
let fold_left_map f accu l =
    let (let*) = Result.bind in
    let rec aux accu l_accu = function
        | [] -> Ok (accu, List.rev l_accu)
        | x :: l ->
            let* accu, x = f accu x in
            aux accu (x :: l_accu) l in
    aux accu [] l

(* Close over the environment variables and return an updated environment
    with the closure included. *)
let set_with_closure env name params body =
    let fun_env = Env.shallow_copy env in
    let fun_literal = Function (fun_env, Some name, params, body) in
    Env.define env name fun_literal

let rec evaluate_expression env expr =
    let (let*) = Result.bind in
    match expr with
    | Literal (Variable name, LineNumber line) ->
        begin match Env.get env name with
        | Ok value -> Ok (env, value)
        | Error message -> error message line
        end
    | Literal (value, _) -> Ok (env, value)
    | Unary (op, subexpr, LineNumber line) ->
        let* env, value = evaluate_expression env subexpr in
        begin match op with
        | Negate ->
            begin match value with
            | Number num -> Ok (env, Number (-.num))
            | _ -> error "Operand must be a number." line
            end
        | LogicalNot -> Ok (env, value |> is_truthy |> not |> of_bool)
        end
    | Grouping (subexpr, _) -> evaluate_expression env subexpr
    | Call (callee_expr, arg_exprs, LineNumber line) ->
        let* env, callee = evaluate_expression env callee_expr in
        let* fun_env, name_opt, params, body = match callee with
        | Function (env, name_opt, params, body) ->
            let num_params = List.length params in
            let num_args = List.length arg_exprs in
            if num_params == num_args then
                Ok (env, name_opt, params, body)
            else
                let message = Printf.sprintf
                    "Expected %d arguments but got %d" num_params num_args in
                error message line
        | _ -> error "Can only call functions and classes." line
        in

        (* Evaluate arg expressions with the environment at the call site. *)
        let* env, args = fold_left_map evaluate_expression env arg_exprs in

        (* The environment for the function body will start with the
           environment at the function declaration site (enabling closures)
           and is augmented by the bound parameters. *)
        let call_env =
            List.fold_left2
                Env.define
                (Env.make ~parent:(Some (ref fun_env)))
                params
                args
        in

        (* If the function is named, add it to the call environment to enable
           recursion. *)
        let call_env = match name_opt with
        | Some name -> set_with_closure call_env name params body
        | None -> call_env
        in

        begin match evaluate_statements call_env body with
        | ER.Ok _ -> Ok (env, Nil)
        | ER.ReturnValue v -> Ok (env, v)
        | ER.Error m -> error m line
        end
    (* Logical and *)
    | Binary (And, subexpr1, subexpr2, LineNumber _) ->
        let* env, l = evaluate_expression env subexpr1 in
        if l |> is_truthy |> not then
            (* Short-circuit *)
            Ok (env, l)
        else
            evaluate_expression env subexpr2
    (* Logical or *)
    | Binary (Or, subexpr1, subexpr2, LineNumber _) ->
        let* env, l = evaluate_expression env subexpr1 in
        if l |> is_truthy then
            (* Short-circuit *)
            Ok (env, l)
        else
            evaluate_expression env subexpr2
    | Binary (op, subexpr1, subexpr2, LineNumber line) ->
        let* env, l = evaluate_expression env subexpr1 in
        let* env, r = evaluate_expression env subexpr2 in
        begin match (op, l, r) with
        | Plus, l, r ->
            begin match (l, r) with
            (* String concatenation *)
            | String l, String r -> Ok (env, String (l ^ r))
            (* Floating point addition *)
            | Number l, Number r -> Ok (env, Number (l +. r))
            | _ -> error "Operands must be two numbers or two strings." line
            end
        (* Remaining floating point operations *)
        | Minus, Number l, Number r -> Ok (env, Number (l -. r))
        | Star, Number l, Number r -> Ok (env, Number (l *. r))
        | Slash, Number l, Number r -> Ok (env, Number (l /. r))
        (* Comparison operations *)
        | Less, Number l, Number r -> Ok (env, (l < r) |> of_bool)
        | LessEqual, Number l, Number r -> Ok (env, (l <= r) |> of_bool)
        | Greater, Number l, Number r -> Ok (env, (l > r) |> of_bool)
        | GreaterEqual, Number l, Number r -> Ok (env, (l >= r) |> of_bool)
        (* Boolean equality operations *)
        | EqualEqual, l, r -> Ok (env, (is_equal l r) |> of_bool)
        | BangEqual, l, r -> Ok (env, (is_equal l r) |> not |> of_bool)
        | _ -> error "Operands must be numbers." line
        end
    | Assignment (name, expr, LineNumber line) ->
        let* env, value = evaluate_expression env expr in
        match Env.set env name value with
        | Ok env -> Ok (env, value)
        | Error message -> error message line

and evaluate_while env cond body =
    let (let*) = ER.bind in
    let* env, cond_value = evaluate_expression env cond |> ER.of_result in
    if is_truthy cond_value then
        let* env, _ = evaluate_statement env body in
        evaluate_while env cond body
    else
        ER.Ok (env, Nil)

and evaluate_statement env stmt =
    let (let*) = ER.bind in
    match stmt with
    | Expression expr ->
        let* env, value = evaluate_expression env expr |> ER.of_result in
        ER.Ok (env, value)
    | If (cond_expr, then_branch, else_branch_opt) ->
        let* env, cond = evaluate_expression env cond_expr |> ER.of_result in
        if is_truthy cond then
            evaluate_statement env then_branch
        else
            begin match else_branch_opt with
            | Some stmt -> evaluate_statement env stmt
            | None -> ER.Ok (env, Nil)
            end
    | While (cond_expr, body) -> evaluate_while env cond_expr body
    | Print expr ->
        let* env, value = evaluate_expression env expr |> ER.of_result in
        value |> literal_to_string |> print_endline;
        ER.Ok (env, Nil)
    | Return None ->
        ER.ReturnValue Nil
    | Return (Some expr) ->
        let* _, value = evaluate_expression env expr |> ER.of_result in
        ER.ReturnValue value
    | Block stmts ->
        let parent_env = env in
        let env = Env.make ~parent:(Some (ref parent_env)) in
        (* For a value to be returned from the program, it must be at the
           top-level of the program, not within a block. We may want to revisit
           this later.*)
        begin match evaluate_statements env stmts with
        | ER.Ok _ -> ER.Ok (parent_env, Nil)
        | ER.ReturnValue _ as v -> v
        | ER.Error _ as e -> e
        end
    | FunctionDeclaration (name, params, body) ->
        let env = set_with_closure env name params body in
        ER.Ok (env, Nil)
    | VariableDeclaration (name, init_expr) ->
        let* env, value = match init_expr with
        | Some expr -> evaluate_expression env expr |> ER.of_result
        | None -> ER.Ok (env, Nil)
        in
        ER.Ok (Env.define env name value, Nil)

and evaluate_statements env = function
    (* In practice, this base case won't be executed unless this function is
       called explicitly with an empty list of statements *)
    | [] -> ER.Ok Nil
    | stmt :: stmts ->
        match evaluate_statement env stmt with
        | ER.Ok (env, output) ->
            begin match stmts with
            (* Return the output if this is the last statement. This enables our
               programs to produce values rather than purely executing for side
               effects.

               This is an intentional diversion from the book, the primary
               motivation being to simplify testing. May revisit later. *)
            | [] -> ER.Ok output
            | _ -> evaluate_statements env stmts
            end
        | ER.ReturnValue _ as v -> v
        | ER.Error _ as e -> e

let evaluate_program env stmts =
    match evaluate_statements env stmts with
    | ER.Ok v -> Ok v
    | ER.ReturnValue v -> Ok v
    | ER.Error m -> Error m
