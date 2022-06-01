type position = {
    (* First character in lexeme being scanned. *)
    start: int;
    (* Character currently being considered. *)
    current: int;
    (* The line in the source of `current`. *)
    line: int;
} [@@deriving show, eq]

(** Return an initialized `position` for scanning. *)
val init : unit -> position

(** Scan `source` for the next token, starting from the given `position`.

    Returns a tuple containing the recognized `token_type` as a `result` along
    with the position in `source` where the token can be extracted. If a parsing
    error occurs, the `result` will contain an `Error` with an appropriate
    message. In the case of `Error`, the returned position will still be valid,
    allowing for multiple errors to be surfaced for a given source string.
*)
val scan_token :
    string -> position -> (Token.token_type, string) result * position

val create_token :
    string -> position -> Token.token_type -> (Token.token, string) result

val update_start : position -> position

val scan_and_extract_token :
    string -> position -> (Token.token, string) result * position

val scan_tokens :
    string -> ?pos:position -> (Token.token, string) result Seq.t
