type t = { lexer : Lexer.t } [@@deriving show]

type error =
  | ExpectedToken of Token.t
  | ExpectedEtype
  | ExpectedKey
  | ExpectedValue
[@@deriving show]

let init input : t =
  let lexer = Lexer.init input in
  { lexer }
;;
