type 'a t
[@@deriving show, eq]

val make : parent:('a t ref option) -> 'a t

val contains : 'a t -> string -> bool

val get_parent : 'a t -> 'a t ref option

(** Set the value for a new or existing variable in the current scope. *)
val define : 'a t -> string -> 'a -> 'a t

(** Set the value for an existing variable in this scope or the applicable
    ancestral scope, returning `Error` if it is had not been previously
    defined. *)
val set : 'a t -> string -> 'a -> ('a t, string) result

val get : 'a t -> string -> ('a, string) result

val shallow_copy : 'a t -> 'a t
