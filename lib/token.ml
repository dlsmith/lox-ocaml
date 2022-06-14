type token_type =
    (* Single-character tokens. *)
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star

    (* One or two character tokens. *)
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual

    (* Literals. *)
    | Identifier of string
    | String of string
    | Number of float

    (* Keywords. *)
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While

    | EOF
[@@deriving show, eq]

(* TODO(dlsmith): Consider renaming this. This is really wrapping the context
   around a token, where `token_type` is a better encapsulation of a token.
   *)
type token = {
    token_type: token_type;
    line: int;
} [@@deriving show, eq]

let as_keyword str =
    match str with
    | "and" -> Some And
    | "class" -> Some Class
    | "else" -> Some Else
    | "false" -> Some False
    | "fun" -> Some Fun
    | "for" -> Some For
    | "if" -> Some If
    | "nil" -> Some Nil
    | "or" -> Some Or
    | "print" -> Some Print
    | "return" -> Some Return
    | "super" -> Some Super
    | "this" -> Some This
    | "true" -> Some True
    | "var" -> Some Var
    | "while" -> Some While
    | _ -> None
