type t =
  { lexer : Lexer.t
  ; token : Token.t option
  }
[@@deriving show]

type error =
  | ExpectedToken of Token.t
  | ExpectedEtype
  | ExpectedKey
  | ExpectedValue
[@@deriving show]
