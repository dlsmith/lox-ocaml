type env_of_literals = Ast.literal Env.t

val evaluate_expression :
    env_of_literals ->
        Ast.expression ->
            (env_of_literals * Ast.literal, string) result

val evaluate_program :
    env_of_literals ->
        Ast.statement list ->
            (env_of_literals * Ast.literal, string) result
