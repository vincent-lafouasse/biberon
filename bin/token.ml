module Value = struct
  type t =
    | Boolean of bool
    | Integer of int
    | String of string
end

type t =
  | Identifier of string
  | Value of Value.t
  | EqualSign
  | Lbrace
  | Rbrace
  | Atsign
  | Eof
