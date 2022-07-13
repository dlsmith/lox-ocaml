type env_of_literals = Evaluation.env_of_literals

val scan_or_error : string -> (Parsing.token_list, string list) result

val run :
    env_of_literals ->
        string ->
            (env_of_literals * Ast.literal, string list) result
