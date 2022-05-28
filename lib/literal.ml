type literal =
    | String of string
    | Number of float
[@@deriving show, eq]

let to_string literal =
    match literal with
        | String value -> value
        | Number value -> Float.to_string value
