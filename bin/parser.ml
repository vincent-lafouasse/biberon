let pp_token_array fmt arr =
  Format.pp_print_list (Position.pp_located Token.pp) fmt (Array.to_list arr)
;;

type t =
  { tokens : Token.t Position.located array [@printer pp_token_array]
  ; index : int
  }
[@@deriving show]

type expected_token = Expected of Token.t [@@deriving show]
type actual_token = Actual of Token.t [@@deriving show]

type error =
  | ExpectedToken of expected_token * actual_token
  | ExpectedEtype of actual_token
  | ExpectedKey of actual_token
  | ExpectedValue of actual_token
  | LexerError of Lexer.error
[@@deriving show]

let token_mismatch expected actual = ExpectedToken (Expected expected, Actual actual)

let init (input : string) : (t, error Position.located) result =
  match Lexer.tokenize input with
  | Ok tokens -> Ok { tokens; index = 0 }
  | Error (err, location) -> Error (LexerError err, location)
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

(* ----------tests---------- *)

let expect cond msg = if not cond then failwith ("FAIL: " ^ msg)

let expect_eq expected actual msg show =
  if expected <> actual
  then (
    Printf.eprintf
      "FAIL: %s\n  expected: %s\n  got:      %s\n"
      msg
      (show expected)
      (show actual);
    exit 1)
;;

let show_unit_result r = [%show: (unit, error Position.located) result] r

let make_parser input =
  match init input with
  | Ok p -> p
  | Error _ -> failwith "make_parser: lex failed"
;;

let test_expect_token_match () =
  let p = make_parser "@" in
  let _p, res = expect_token p Token.AtSign in
  expect_eq (Ok ()) res "atsign matches" show_unit_result;
  let p = make_parser "{" in
  let _p, res = expect_token p Token.Lbrace in
  expect_eq (Ok ()) res "lbrace matches" show_unit_result;
  let p = make_parser "=" in
  let _p, res = expect_token p Token.EqualSign in
  expect_eq (Ok ()) res "equalsign matches" show_unit_result
;;

let test_expect_token_mismatch () =
  let p = make_parser "{" in
  let _p, res = expect_token p Token.AtSign in
  expect (Result.is_error res) "lbrace does not match atsign";
  let p = make_parser "foo" in
  let _p, res = expect_token p Token.Lbrace in
  expect (Result.is_error res) "ident does not match lbrace"
;;

let test_expect_token_advances () =
  let p = make_parser "@{" in
  let p, res = expect_token p Token.AtSign in
  expect_eq (Ok ()) res "first token ok" show_unit_result;
  let _p, res = expect_token p Token.Lbrace in
  expect_eq (Ok ()) res "second token ok after advance" show_unit_result
;;

let test_expect_token_eof () =
  let p = make_parser "" in
  let _p, res = expect_token p Token.AtSign in
  expect (Result.is_error res) "eof does not match atsign"
;;

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () =
  run_test "expect_token: match" test_expect_token_match;
  run_test "expect_token: mismatch" test_expect_token_mismatch;
  run_test "expect_token: advances" test_expect_token_advances;
  run_test "expect_token: eof" test_expect_token_eof
;;
