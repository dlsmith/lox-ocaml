type literal =
    | Number of float
    | String of string
    | Variable of string
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

(* TODO(dlsmith): We include a line number to enable error reporting. This does
   not feel like a good way to carry around this information, but I'm taking
   the most direct path for now to not overcomplicate things, until I know
   better how I want to generalize. This _will_ be refactored.
 *)
type expression =
    | Literal of literal * line_number
    | Unary of unary_op * expression * line_number
    | Binary of binary_op * expression * expression * line_number
    | Grouping of expression * line_number
    | Assignment of string * expression * line_number

type statement =
    | Expression of expression
    | If of expression * statement * (statement option)
    | Print of expression
    | Block of statement list
    | VariableDeclaration of string * (expression option)

(* TODO(dlsmith): Use the `show` function on the type *)
let literal_to_string = function
    | Number num -> Float.to_string num
    | String str -> str
    | Variable name -> Printf.sprintf "(var %s)" name
    | True -> "true"
    | False -> "false"
    | Nil -> "nil"

(** Serialize an `expression` AST as an s-expression. *)
let rec expr_to_sexp = function
    | Literal (literal, _) -> literal_to_string literal
    | Unary (op, subexpr, _) ->
        let op_str = match op with
        | Negate -> "-"
        | LogicalNot -> "!"
        in
        Printf.sprintf "(%s %s)" op_str (expr_to_sexp subexpr)
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
            (expr_to_sexp subexpr1)
            (expr_to_sexp subexpr2)
    | Grouping (subexpr, _) ->
        Printf.sprintf "(group %s)" (expr_to_sexp subexpr)
    | Assignment (name, expr, _) ->
        Printf.sprintf "(assign %s %s)" name (expr_to_sexp expr)

let rec stmt_to_sexp = function
    | Expression expr ->
        Printf.sprintf "(expr %s)" (expr_to_sexp expr)
    | If (condition, then_branch, None) ->
        Printf.sprintf
            "(if %s %s)"
            (expr_to_sexp condition)
            (stmt_to_sexp then_branch)
    | If (condition, then_branch, Some else_branch) ->
        Printf.sprintf
            "(if %s %s %s)"
            (expr_to_sexp condition)
            (stmt_to_sexp then_branch)
            (stmt_to_sexp else_branch)
    | Print expr ->
        Printf.sprintf "(print %s)" (expr_to_sexp expr)
    | VariableDeclaration (name, Some expr) ->
        Printf.sprintf "(var-decl %s %s)" name (expr_to_sexp expr)
    | VariableDeclaration (name, None) ->
        Printf.sprintf "(var-decl %s)" name
    | Block stmts ->
        Printf.sprintf "(block %s)" (stmts_to_sexp stmts)

and stmts_to_sexp stmts =
    let sexps = List.map stmt_to_sexp stmts in
    match sexps with
    | [] -> ""
    | [sexp] -> sexp
    | sexp :: sexps ->
        Printf.sprintf
            "(%s)"
            (List.fold_left (fun a b -> a ^ " " ^ b)
            sexp
            sexps)
