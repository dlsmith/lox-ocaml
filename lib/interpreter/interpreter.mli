val scan_or_error : string -> (Parsing.token_list, string) result

val run : string -> (Ast.literal, string) result

val report : int -> string -> string -> unit

val error : int -> string -> unit
