val evaluate_expression : Parsing.expression -> (Parsing.literal, string) result

val evaluate_statement : Parsing.statement -> (unit, string) result

val evaluate_program : Parsing.statement list -> (unit, string) result
