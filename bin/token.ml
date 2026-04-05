module Value = struct
  type t =
    | Boolean of bool
    | Integer of int
    | String of string
  [@@deriving show]

  let format (value : t) : string =
    match value with
    | String s -> Printf.sprintf "string \"%s\"" s
    | Integer i -> Printf.sprintf "integer '%d'" i
    | Boolean b -> Printf.sprintf "boolean '%b'" b
  ;;
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

let format (token : t) : string =
  match token with
  | Identifier ident -> Printf.sprintf "identifier \"%s\"" ident
  | Value value -> Value.format value
  | EqualSign -> "'='"
  | Lbrace -> "'{'"
  | Rbrace -> "'}'"
  | AtSign -> "'@'"
  | Comma -> "','"
  | Eof -> "end of file"
;;
