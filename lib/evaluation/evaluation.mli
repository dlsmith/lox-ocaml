module Env : sig
    type t

    val make : parent:(t ref option) -> t

    val contains : t -> string -> bool

    val get_parent : t -> t ref option

    (** Set the value for a new or existing variable in the current scope. *)
    val define : t -> string -> Ast.literal -> t

    (** Set the value for an existing variable in this scope or the applicable
        ancestral scope, returning `Error` if it is had not been previously
        defined. *)
    val set : t -> string -> Ast.literal -> (t, string) result

    val get : t -> string -> (Ast.literal, string) result
end

val evaluate_expression :
    Env.t ->
        Ast.expression ->
            (Env.t * Ast.literal, string) result

val evaluate_program :
    Env.t ->
        Ast.statement list ->
            (Ast.literal, string) result
