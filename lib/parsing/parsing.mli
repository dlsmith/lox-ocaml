exception Parse_error of string

type token_list = Token.token list

val parse_expression :
    token_list ->
        (Ast.expression * token_list, string * token_list) result

val parse_statement :
    token_list ->
        (Ast.statement * token_list, string * token_list) result

val parse_declaration :
    token_list ->
        (Ast.statement * token_list, string * token_list) result

val parse_program :
    token_list ->
        (Ast.statement, string) result list
