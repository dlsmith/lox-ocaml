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
    | Print of expression
    | Block of statement list
    | VariableDeclaration of string * (expression option)

val literal_to_string : literal -> string

(** Serialize an `expression` AST as an s-expression. *)
(* TODO(dlsmith): Probably shouldn't live in `Parsing` module. *)
val to_sexp : expression -> string

exception Parse_error of string

type token_list = Token.token list

val parse_expression :
    token_list ->
        (expression * token_list, string * token_list) result

val parse_statement :
    token_list ->
        (statement * token_list, string * token_list) result

val parse_declaration :
    token_list ->
        (statement * token_list, string * token_list) result

val parse_program :
    token_list ->
        (statement, string) result list
