module Env : sig
    type t

    val make : parent:(t ref option) -> t

    val contains : t -> string -> bool

    val get_parent : t -> t ref option

    (** Set the value for a new or existing variable in the current scope. *)
    val define : t -> string -> Parsing.literal -> t

    (** Set the value for an existing variable in this scope or the applicable
        ancestral scope, returning `Error` if it is had not been previously
        defined. *)
    val set : t -> string -> Parsing.literal -> (t, string) result

    val get : t -> string -> (Parsing.literal, string) result
end

val evaluate_expression :
    Env.t ->
        Parsing.expression ->
            (Parsing.literal * Env.t, string) result

val evaluate_statement :
    Env.t ->
        Parsing.statement ->
            (Parsing.literal option * Env.t, string) result

val evaluate_program :
    Env.t ->
        Parsing.statement list ->
            (Parsing.literal option, string) result
