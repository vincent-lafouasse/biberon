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
  | LexerError of Lexer.error
[@@deriving show]

let init input : t =
  let lexer = Lexer.init input in
  { lexer }
;;

let init (input : string) : (t, error Position.located) result =
  match Lexer.tokenie input with
  | Ok tokens -> { tokens }
  | Error err -> Error (LexerError err)
;;

(* main export probably *)
let parse (input : string) : (Entry.raw_entry Array.t, error Position.located) result =
  failwith "unimplemented"
;;
