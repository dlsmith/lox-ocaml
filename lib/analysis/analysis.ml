open Ast

(* TODO(dlsmith): Lox also uses analysis to detect re-definitin of variables
   in local scope (it's allowed in global scope for convenience). Consider
   including that, especially if we build out analyses more to make it easier.
   It wouldn't be possible with the analysis code below since we're not keeping
   track of declaration state as they do. *)

let (let*) = Result.bind

let rec analyze_expr = function
    | Literal _ -> Ok ()
    | Unary (_, expr, _) -> analyze_expr expr
    | Binary (_, expr1, expr2, _) ->
        Util.map_or_error analyze_expr [expr1; expr2]
    | Grouping (expr, _) -> analyze_expr expr
    | Assignment (_, expr, _) -> analyze_expr expr
    | Call (callee_expr, arg_exprs, _) ->
        Util.map_or_error analyze_expr (callee_expr :: arg_exprs)

and analyze_stmt = function
    | Expression expr -> analyze_expr expr
    | If (cond, then_branch, else_branch_opt) ->
        let* _ = analyze_expr cond in
        let* _ = analyze_stmt then_branch in
        begin match else_branch_opt with
        | Some else_branch -> analyze_stmt else_branch
        | None -> Ok ()
        end
    | Print expr -> analyze_expr expr
    | Return _ -> Error "Can't return from top-level code."
    | While (cond, body) -> let* _ = analyze_expr cond in analyze_stmt body
    | Block stmts -> Util.map_or_error analyze_stmt stmts
    | FunctionDeclaration (_, _, _) -> Ok ()
    | VariableDeclaration (_, expr_opt) ->
        match expr_opt with
        | Some expr -> analyze_expr expr
        | None -> Ok ()

let analyze_return (stmts: statement list) : (statement, string) result list =
    (* We just need to check for use of `return` statements at the top-level,
       i.e., before we enter a function body. We'll simply walk to the AST
       checking for a return statement, and breaking out of the walk as soon
       as we enter a function body (after which `return` becomes valid). *)

    let apply_analyze stmt =
        analyze_stmt stmt
        |> Result.map (fun () -> stmt)
    in
    List.map apply_analyze stmts
