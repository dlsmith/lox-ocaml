val read_file : string -> string

val interpret_file : string -> (string option, string) result

val interpret_interactive : unit -> (string option, string) result
