type t =
  { tokens : Token.t Position.located Array.t
  ; index : int
  }
[@@deriving show]

type expected_token = Expected of Token.t
type actual_token = Actual of Token.t

type error =
  | ExpectedToken of expected_token * actual_token
  | ExpectedEtype of actual_token
  | ExpectedKey of actual_token
  | ExpectedValue of actual_token
  | LexerError of Lexer.error
[@@deriving show]

let token_mismatch expected actual = ExpectedToken (Expected expected, Actual actual)

let init (input : string) : (t, error Position.located) result =
  match Lexer.tokenie input with
  | Ok tokens -> { tokens }
  | Error err -> Error (LexerError err)
;;

let get parser = Array.get parser.tokens parser.index

let advance parser =
  let index = parser.index + 1 in
  { parser with index }
;;

let expect_token parser expected : t * (unit, error Position.located) result =
  let actual, location = get parser in
  match actual with
  | expected -> advance parser, Ok ()
  | _ -> parser, Error (token_mismatch expected actual, location)
;;

(* main export probably *)
let parse (input : string) : (Entry.raw_entry Array.t, error Position.located) result =
  let parser_res = init input in
  (* gather entries while parser_res is ok *)
  failwith "unimplemented"
;;
