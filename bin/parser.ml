type t =
  { tokens : Token.t Position.located Array.t
  ; index : int
  }
[@@deriving show]

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

let init (input : string) : (t, Lexer.error Position.located) result = failwith "todo"
