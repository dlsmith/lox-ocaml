val evaluate_expression :
    Ast.literal Env.t ->
        Ast.expression ->
            (Ast.literal Env.t * Ast.literal, string) result

val evaluate_program :
    Ast.literal Env.t ->
        Ast.statement list ->
            (Ast.literal, string) result
