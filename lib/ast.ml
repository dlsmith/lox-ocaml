type unary_op =
    | Negate
    | LogicalNot
[@@deriving show, eq]

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
    | And
    | Or
[@@deriving show, eq]

type line_number = LineNumber of int [@@deriving show, eq]

type literal =
    | Number of float
    | String of string
    | Variable of string
    | Function of string list * statement list
    | True
    | False
    | Nil
[@@deriving show, eq]

(* TODO(dlsmith): We include a line number to enable error reporting. This does
   not feel like a good way to carry around this information, but I'm taking
   the most direct path for now to not overcomplicate things, until I know
   better how I want to generalize. This _will_ be refactored.
 *)
and expression =
    | Literal of literal * line_number
    | Unary of unary_op * expression * line_number
    | Binary of binary_op * expression * expression * line_number
    | Call of expression * expression list * line_number
    | Grouping of expression * line_number
    | Assignment of string * expression * line_number
[@@deriving show, eq]

and statement =
    | Expression of expression
    | If of expression * statement * (statement option)
    | Print of expression
    | Return of expression option
    | While of expression * statement
    | Block of statement list
    | FunctionDeclaration of string * string list * statement list
    | VariableDeclaration of string * (expression option)
[@@deriving show, eq]

(* TODO(dlsmith): Use the `show` function on the type *)
let rec literal_to_string = function
    | Number num -> Float.to_string num
    | String str -> Printf.sprintf "\"%s\"" str
    | Variable name -> Printf.sprintf "(var %s)" name
    | Function (params, body) ->
        Printf.sprintf
            "(fun (%s) (%s))"
            (String.concat " " params)
            (body |> List.map stmt_to_sexp |> String.concat " ")
    | True -> "true"
    | False -> "false"
    | Nil -> "nil"

(** Serialize an `expression` AST as an s-expression. *)
and expr_to_sexp = function
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
        | And -> "and"
        | Or -> "or"
        in
        Printf.sprintf
            "(%s %s %s)"
            op_str
            (expr_to_sexp subexpr1)
            (expr_to_sexp subexpr2)
    | Call (callee, args, _) ->
        Printf.sprintf
            "(call %s (%s))"
            (expr_to_sexp callee)
            (args |> List.map expr_to_sexp |> String.concat " ")
    | Grouping (subexpr, _) ->
        Printf.sprintf "(group %s)" (expr_to_sexp subexpr)
    | Assignment (name, expr, _) ->
        Printf.sprintf "(assign %s %s)" name (expr_to_sexp expr)

and stmt_to_sexp = function
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
    | While (condition, body) ->
        Printf.sprintf
            "(while %s %s)"
            (expr_to_sexp condition)
            (stmt_to_sexp body)
    | Print expr ->
        Printf.sprintf "(print %s)" (expr_to_sexp expr)
    | Return (Some expr) ->
        Printf.sprintf "(return %s)" (expr_to_sexp expr)
    | Return None -> "(return nil)"
    | FunctionDeclaration (name, params, body) ->
        Printf.sprintf
            "(fun-decl %s (%s) (%s))"
            name
            (String.concat " " params)
            (body |> List.map stmt_to_sexp |> String.concat " ")
    | VariableDeclaration (name, Some expr) ->
        Printf.sprintf "(var-decl %s %s)" name (expr_to_sexp expr)
    | VariableDeclaration (name, None) ->
        Printf.sprintf "(var-decl %s)" name
    | Block stmts ->
        Printf.sprintf "(block %s)" (stmts_to_sexp stmts)

and stmts_to_sexp stmts =
    stmts
    |> List.map stmt_to_sexp
    |> String.concat " "
    |> Printf.sprintf "(%s)"
