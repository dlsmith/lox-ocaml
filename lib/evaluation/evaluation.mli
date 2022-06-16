module Env : sig
    type t
    val make : unit -> t
    val define : t -> string -> Parsing.literal -> t
    val get : t -> string -> (Parsing.literal, string) result
end

val evaluate_expression :
    Env.t -> Parsing.expression -> (Parsing.literal, string) result

val evaluate_statement :
    Env.t ->
        Parsing.statement ->
            (Parsing.literal option * Env.t, string) result

val evaluate_program :
    Env.t ->
        Parsing.statement list ->
            (Parsing.literal option, string) result
