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
  | ExpectedTag of actual_token
  | ExpectedEtype of actual_token
  | ExpectedKey of actual_token
  | ExpectedValue of actual_token
  | LexerError of Lexer.error
[@@deriving show]

let token_mismatch ~expected ~actual = ExpectedToken (Expected expected, Actual actual)

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

(* yeah there's a bunch of duplication, i do not care *)

let expect_token parser expected : t * (unit, error Position.located) result =
  let actual, location = get parser in
  match actual with
  | tok when tok = expected -> advance parser, Ok ()
  | _ -> parser, Error (token_mismatch ~expected ~actual, location)
;;

let expect_etype parser : t * (Entry.etype, error Position.located) result =
  let actual, location = get parser in
  match actual with
  | Token.Identifier etype -> advance parser, Ok (Entry.Etype etype)
  | _ -> parser, Error (ExpectedEtype (Actual actual), location)
;;

let expect_tag parser : t * (Entry.tag, error Position.located) result =
  let actual, location = get parser in
  match actual with
  | Token.Identifier etype -> advance parser, Ok (Entry.Tag etype)
  | _ -> parser, Error (ExpectedTag (Actual actual), location)
;;

let expect_key parser : t * (Entry.key, error Position.located) result =
  let actual, location = get parser in
  match actual with
  | Token.Identifier key -> advance parser, Ok (Entry.Key key)
  | _ -> parser, Error (ExpectedKey (Actual actual), location)
;;

let expect_value parser : t * (Entry.Value.t, error Position.located) result =
  let transmute value =
    match value with
    | Token.Value.Boolean b -> Entry.Value.Boolean b
    | Token.Value.Integer i -> Entry.Value.Integer i
    | Token.Value.String s -> Entry.Value.String s
  in
  let actual, location = get parser in
  match actual with
  | Token.Value value -> advance parser, Ok (transmute value)
  | _ -> parser, Error (ExpectedValue (Actual actual), location)
;;

let expect_field parser : t * (Entry.field, error Position.located) result =
  let past_key_parser, key_res = expect_key parser in
  let parser = if Result.is_ok key_res then past_key_parser else parser in
  let past_equal_parser, equalsign_res =
    match key_res with
    | Ok _ -> expect_token parser Token.EqualSign
    | Error (err, loc) -> parser, Error (err, loc)
  in
  let parser = if Result.is_ok equalsign_res then past_equal_parser else parser in
  let past_value_parser, value_res =
    match equalsign_res with
    | Ok () -> expect_value parser
    | Error (err, loc) -> parser, Error (err, loc)
  in
  let parser = if Result.is_ok value_res then past_value_parser else parser in
  match value_res with
  | Error (err, loc) -> parser, Error (err, loc)
  | Ok value ->
    (* should be safe *)
    let key = Result.get_ok key_res in
    parser, Ok (key, value)
;;

let expect_field_trailing_comma parser : t * (Entry.field, error Position.located) result =
  let past_field_parser, field_res = expect_field parser in
  let parser = if Result.is_ok field_res then past_field_parser else parser in
  let past_comma_parser, comma_res =
    match field_res with
    | Error (err, loc) -> parser, Error (err, loc)
    | Ok _ -> expect_token parser Token.Comma
  in
  let parser = if Result.is_ok comma_res then past_comma_parser else parser in
  match comma_res with
  | Error (err, loc) -> parser, Error (err, loc)
  | Ok () ->
    let field = Result.get_ok field_res in
    parser, Ok field
;;

let expect_field_list parser : t * (Entry.field list, error Position.located) result =
  let (*rec*) _fold_field_list parser acc =
    let _ = parser, acc in
    failwith "todo"
  in
  let _ = parser in
  failwith "todo"
;;

let expect_entry parser : t * (Entry.raw_entry, error Position.located) result =
  let past_atsign_parser, atsign_res = expect_token parser Token.AtSign in
  let parser = if Result.is_ok atsign_res then past_atsign_parser else parser in
  let past_lbrace_parser, lbrace_res = expect_token parser Token.Lbrace in
  let parser = if Result.is_ok lbrace_res then past_lbrace_parser else parser in
  let past_tag_parser, tag_res = expect_tag parser in
  let parser = if Result.is_ok tag_res then past_tag_parser else parser in
  (* parse the fields here *)
  let past_rbrace_parser, rbrace_res = expect_token parser Token.Rbrace in
  let parser = if Result.is_ok tag_res then past_rbrace_parser else parser in
  match rbrace_res with
  | Error (err, loc) -> parser, Error (err, loc)
  | Ok () ->
    let _tag = Result.get_ok tag_res in
    let _fields : Entry.field list = [] in
    (* TODO: actually parse the fields *)
    failwith "unimplemented"
;;

(* main export probably *)
let parse (input : string) : (Entry.raw_entry Array.t, error Position.located) result =
  let _parser_res = init input in
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

let pos a l c : Position.t = { absolute = a; line = l; column = c }

let test_expect_token_mismatch () =
  let p = make_parser "{" in
  let _p, res = expect_token p Token.AtSign in
  expect_eq
    (Error (token_mismatch ~expected:Token.AtSign ~actual:Token.Lbrace, pos 0 1 0))
    res
    "lbrace does not match atsign"
    show_unit_result;
  let p = make_parser "foo" in
  let _p, res = expect_token p Token.Lbrace in
  expect_eq
    (Error
       (token_mismatch ~expected:Token.Lbrace ~actual:(Token.Identifier "foo"), pos 0 1 0))
    res
    "ident does not match lbrace"
    show_unit_result
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
  expect_eq
    (Error (token_mismatch ~expected:Token.AtSign ~actual:Token.Eof, pos 0 1 0))
    res
    "eof does not match atsign"
    show_unit_result
;;

let show_etype_result r = [%show: (Entry.etype, error Position.located) result] r
let show_key_result r = [%show: (Entry.key, error Position.located) result] r
let show_value_result r = [%show: (Entry.Value.t, error Position.located) result] r

let test_expect_etype () =
  let p = make_parser "article" in
  let _p, res = expect_etype p in
  expect_eq (Ok (Entry.Etype "article")) res "identifier becomes etype" show_etype_result;
  let p = make_parser "inproceedings" in
  let _p, res = expect_etype p in
  expect_eq (Ok (Entry.Etype "inproceedings")) res "inproceedings etype" show_etype_result;
  let p = make_parser "{" in
  let _p, res = expect_etype p in
  expect_eq
    (Error (ExpectedEtype (Actual Token.Lbrace), pos 0 1 0))
    res
    "lbrace is not an etype"
    show_etype_result;
  let p = make_parser "" in
  let _p, res = expect_etype p in
  expect_eq
    (Error (ExpectedEtype (Actual Token.Eof), pos 0 1 0))
    res
    "eof is not an etype"
    show_etype_result
;;

let test_expect_key () =
  let p = make_parser "doe2024" in
  let _p, res = expect_key p in
  expect_eq (Ok (Entry.Key "doe2024")) res "identifier becomes key" show_key_result;
  let p = make_parser "smith_2023" in
  let _p, res = expect_key p in
  expect_eq (Ok (Entry.Key "smith_2023")) res "key with underscore" show_key_result;
  let p = make_parser "{" in
  let _p, res = expect_key p in
  expect_eq
    (Error (ExpectedKey (Actual Token.Lbrace), pos 0 1 0))
    res
    "lbrace is not a key"
    show_key_result;
  let p = make_parser "" in
  let _p, res = expect_key p in
  expect_eq
    (Error (ExpectedKey (Actual Token.Eof), pos 0 1 0))
    res
    "eof is not a key"
    show_key_result
;;

let test_expect_value () =
  let p = make_parser {|"hello"|} in
  let _p, res = expect_value p in
  expect_eq (Ok (Entry.Value.String "hello")) res "string value" show_value_result;
  let p = make_parser "2024" in
  let _p, res = expect_value p in
  expect_eq (Ok (Entry.Value.Integer 2024)) res "integer value" show_value_result;
  let p = make_parser "true" in
  let _p, res = expect_value p in
  expect_eq (Ok (Entry.Value.Boolean true)) res "boolean true" show_value_result;
  let p = make_parser "false" in
  let _p, res = expect_value p in
  expect_eq (Ok (Entry.Value.Boolean false)) res "boolean false" show_value_result;
  let p = make_parser "{" in
  let _p, res = expect_value p in
  expect_eq
    (Error (ExpectedValue (Actual Token.Lbrace), pos 0 1 0))
    res
    "lbrace is not a value"
    show_value_result;
  let p = make_parser "" in
  let _p, res = expect_value p in
  expect_eq
    (Error (ExpectedValue (Actual Token.Eof), pos 0 1 0))
    res
    "eof is not a value"
    show_value_result
;;

let show_field_result r = [%show: (Entry.field, error Position.located) result] r

let test_expect_field () =
  (* happy paths *)
  let _p, res = expect_field (make_parser {|author = "Doe, J."|}) in
  expect_eq
    (Ok (Entry.Key "author", Entry.Value.String "Doe, J."))
    res
    "string field"
    show_field_result;
  let _p, res = expect_field (make_parser "year = 2024") in
  expect_eq
    (Ok (Entry.Key "year", Entry.Value.Integer 2024))
    res
    "integer field"
    show_field_result;
  let _p, res = expect_field (make_parser "reviewed = true") in
  expect_eq
    (Ok (Entry.Key "reviewed", Entry.Value.Boolean true))
    res
    "boolean field"
    show_field_result;
  (* parser advances past the whole field *)
  let p, res = expect_field (make_parser {|a = "x", b = "y"|}) in
  expect (Result.is_ok res) "advances: no error";
  (match get p with
   | Token.Comma, _ -> ()
   | tok, _ ->
     expect
       false
       (Printf.sprintf "advances: expected comma next, got %s" (Token.show tok)));
  let err_variant res =
    match res with
    | Error (e, _) -> Some e
    | Ok _ -> None
  in
  (* error: missing key *)
  let _p, res = expect_field (make_parser {|= "value"|}) in
  expect_eq
    (Some (ExpectedKey (Actual Token.EqualSign)))
    (err_variant res)
    "missing key: error variant"
    [%show: error option];
  (* error: missing equals *)
  let _p, res = expect_field (make_parser {|key "value"|}) in
  expect_eq
    (Some
       (ExpectedToken
          (Expected Token.EqualSign, Actual (Token.Value (Token.Value.String "value")))))
    (err_variant res)
    "missing equals: error variant"
    [%show: error option];
  (* error: missing value *)
  let _p, res = expect_field (make_parser "key =") in
  expect_eq
    (Some (ExpectedValue (Actual Token.Eof)))
    (err_variant res)
    "missing value: error variant"
    [%show: error option];
  (* error: eof *)
  let _p, res = expect_field (make_parser "") in
  expect_eq
    (Some (ExpectedKey (Actual Token.Eof)))
    (err_variant res)
    "eof: error variant"
    [%show: error option]
;;

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () =
  run_test "expect_token: match" test_expect_token_match;
  run_test "expect_token: mismatch" test_expect_token_mismatch;
  run_test "expect_token: advances" test_expect_token_advances;
  run_test "expect_token: eof" test_expect_token_eof;
  run_test "expect_etype" test_expect_etype;
  run_test "expect_key" test_expect_key;
  run_test "expect_value" test_expect_value;
  run_test "expect_field" test_expect_field
;;
