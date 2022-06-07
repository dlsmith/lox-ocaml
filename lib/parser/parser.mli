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

(* TODO: Unary and Binary operations need to carry code reference payload for
    error handling. The book passes the op token through as part of the
    expression payload. I initially tried this, but it doesn't end up taking
    much advantage of the type system. I.e., we have to check the token type
    when we create the `expression`, but then store the op as a generic token
    and then have to keep checking it again when we interpret the AST. Instead,
    we should make illegal states unrepresentable, and also just pass through
    what we need (e.g., line number or some `code_ref` record type).
*)
type expression =
    | Literal of literal
    | Unary of unary_op * expression
    | Binary of binary_op * expression * expression
    | Grouping of expression

(** Serialize an `expression` AST as an s-expression. *)
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

val evaluate_expression : expression -> literal
