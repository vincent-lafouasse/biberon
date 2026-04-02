module Value = struct
  type t =
    | Boolean of bool
    | Integer of int
    | String of string
  [@@deriving show]
end

type t =
  | Identifier of string
  | Value of Value.t
  | EqualSign
  | Lbrace
  | Rbrace
  | AtSign
  | Comma
  | Eof
[@@deriving show]
