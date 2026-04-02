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

let next_token parser = Lexer.next_token parser.lexer

let next_raw_entry parser : t * (Entry.raw_entry, error Position.located) result =
  let _ = parser in
  failwith "unimplemented"
;;
