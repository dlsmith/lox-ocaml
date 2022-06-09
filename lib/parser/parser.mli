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

type line_number = LineNumber of int

(* TODO(dlsmith): For some expression variants we include a line number to
   enable error reporting. This does not feel like a good way to carry around
   this information, but I'm taking the most direct path for now to not
   overcomplicate things, until I know better how I want to generalize.
   This _will_ be refactored.
 *)
type expression =
    | Literal of literal
    | Unary of unary_op * expression * line_number
    | Binary of binary_op * expression * expression * line_number
    | Grouping of expression

(** Serialize an `expression` AST as an s-expression. *)
(* TODO(dlsmith): Probably shouldn't live in `Parser` module. *)
val to_sexp : expression -> string

exception Parse_error of string

type token_list = Token.token list

type partial_parse = (expression * token_list, string * token_list) result

type expression_parser = token_list -> partial_parse

val parse_primary : expression_parser

val parse_unary : expression_parser

(* TODO(dlsmith): I don't actually want to expose this from the module, but
   for now I want to keep the signature explicit. *)
val parse_left_assoc_binary_ops :
    subparser:expression_parser ->
        (Token.token_type -> binary_op option) ->
            token_list ->
                partial_parse

val parse_expression : expression_parser

(* TODO(dlsmith): Probably shouldn't live in `Parser` module. *)
val evaluate_expression : expression -> (literal, string) result
