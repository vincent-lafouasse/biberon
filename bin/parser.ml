[@@@warning "-69-34-37-32"]

type position =
  { absolute : int
  ; line : int
  ; column : int
  }
[@@deriving show]

let distance pos_from pos_to = pos_to.absolute - pos_from.absolute

type t =
  { input : string
  ; position : position
  }
[@@deriving show]

type error =
  | UnexpectedCharacter of char
  | UnexpectedEof
  | InvalidKeyFirstCharacter of char
  | InvalidKeyCharacter of char
[@@deriving show]

let init (input : string) : t =
  let position = { absolute = 0; line = 1; column = 0 } in
  { input; position }
;;

let len parser = String.length parser.input

let at parser position = String.get parser.input position.absolute

let eof parser = parser.position.absolute >= len parser

let get (parser : t) : char option =
  if eof parser then None else Some (at parser parser.position)
;;

let get_unsafe parser : char = Option.get (get parser)

let increment_position position break_line =
  let absolute = position.absolute + 1 in
  let line, column =
    if break_line then position.line + 1, 0 else position.line, position.column + 1
  in
  { absolute; line; column }
;;

let advance parser =
  let can_advance = not (eof parser) in
  let break_line = can_advance && Char.equal '\n' (get_unsafe parser) in
  let position = parser.position in
  let position =
    if can_advance then increment_position position break_line else position
  in
  { parser with position }
;;

let peek (parser : t) : char option =
  let parser = advance parser in
  get parser
;;

let rec advance_by parser offset : t =
  if offset > 0 then advance_by (advance parser) (offset - 1) else parser
;;

let either f g x = f x || g x

let expect_char (parser : t) (pred : char -> bool) : t * error option =
  match get parser with
  | Some c when pred c -> advance parser, None
  | Some c -> parser, Some (UnexpectedCharacter c)
  | None -> parser, Some UnexpectedEof
;;

let expect_char_eq (parser : t) (c : char) : t * error option =
  expect_char parser (Char.equal c)
;;

let char_is_ident_start : char -> bool = either Char.Ascii.is_letter (Char.equal '_')

let rec advance_while pred parser =
  let continue = (not (eof parser)) && pred (get_unsafe parser) in
  if continue then advance_while pred (advance parser) else parser
;;

let find_word_end = advance_while (either Char.Ascii.is_alphanum (Char.equal '_'))

let expect_identifier (parser : t) : t * (string, error) result =
  let start : position = parser.position in
  let parser, first_char_res =
    match get parser with
    | None -> parser, Error UnexpectedEof
    | Some c when char_is_ident_start c -> advance parser, Ok ()
    | Some c -> parser, Error (InvalidKeyFirstCharacter c)
  in
  let parser, end_position_res =
    match first_char_res with
    | Error err -> parser, Error err
    | Ok _ ->
      let past_end_parser = find_word_end parser in
      past_end_parser, Ok past_end_parser.position
  in
  let identifier_res =
    match end_position_res with
    | Error err -> Error err
    | Ok end_pos -> Ok (String.sub parser.input start.absolute (distance start end_pos))
  in
  parser, identifier_res
;;

let skip_whitespace = advance_while Char.Ascii.is_white

(* parser -> parser * Entry.raw_entry option *)
let next_raw_entry (_parser : t) = failwith "todo"

let log (parser : t) : unit = print_endline (show parser)

(* ----------tests---------- *)

let expect cond msg = if not cond then failwith ("FAIL: " ^ msg)

let expect_eq expected actual msg show =
  if expected <> actual
  then (
    let message =
      Printf.sprintf
        "FAIL: %s\n  expected: %s\n  got:      %s"
        msg
        (show expected)
        (show actual)
    in
    let () = Printf.eprintf "FAIL: %s\n" message in
    failwith "assertion failed")
;;

let show_char_opt c = [%show: char option] c
let show_error_opt e = [%show: error option] e
let show_result r = [%show: (string, error) result] r

let test_init () =
  let empty = init "" in
  expect (eof empty) "empty: eof";
  expect_eq None (get empty) "empty: ch=None" show_char_opt;
  let p = init "abc" in
  expect_eq (Some 'a') (get p) "ch=Some 'a'" show_char_opt;
  expect (not (eof p)) "not eof"
;;

let test_peek () =
  let p = init "abc" in
  expect_eq (Some 'b') (peek p) "peek first" show_char_opt;
  expect_eq None (peek (advance (advance p))) "peek at last char" show_char_opt;
  expect_eq None (peek (init "")) "peek at eof" show_char_opt
;;

let test_advance () =
  let p = init "abc" in
  let p1 = advance p in
  expect_eq (Some 'b') (get p1) "ch=Some 'b'" show_char_opt;
  expect_eq (Some 'c') (peek p1) "peek=Some 'c'" show_char_opt;
  let p2 = advance p1 in
  expect_eq (Some 'c') (get p2) "ch=Some 'c'" show_char_opt;
  expect_eq None (peek p2) "peek=None at end" show_char_opt;
  let p3 = advance p2 in
  expect (eof p3) "eof";
  expect_eq None (get p3) "ch=None past end" show_char_opt;
  let p4 = advance p3 in
  expect (p3 = p4) "idempotent at eof"
;;

let test_advance_by () =
  expect_eq (Some 'd') (get (advance_by (init "abcde") 3)) "advance_by 3" show_char_opt;
  expect (eof (advance_by (init "abc") 3)) "advance_by to end: eof";
  expect (eof (advance_by (init "abc") 99)) "advance_by past end: eof";
  expect_eq
    (Some 'a')
    (get (advance_by (init "abc") 0))
    "advance_by 0: no change"
    show_char_opt
;;

let test_expect_char () =
  let p, err = expect_char (init "abc") Char.Ascii.is_letter in
  expect_eq None err "match: no error" show_error_opt;
  expect_eq (Some 'b') (get p) "match: advanced" show_char_opt;
  let p, err = expect_char (init "abc") Char.Ascii.is_digit in
  expect_eq (Some (UnexpectedCharacter 'a')) err "no match: error" show_error_opt;
  expect_eq (Some 'a') (get p) "no match: not advanced" show_char_opt;
  let _, err = expect_char (init "") Char.Ascii.is_letter in
  expect_eq (Some UnexpectedEof) err "eof: UnexpectedEof" show_error_opt
;;

let test_expect_identifier () =
  let p, res = expect_identifier (init "abc") in
  expect_eq (Ok "abc") res "happy: Ok abc" show_result;
  expect (eof p) "happy: parser at end";
  let p, res = expect_identifier (init "foo_bar baz") in
  expect_eq (Ok "foo_bar") res "stops at space" show_result;
  expect_eq (Some ' ') (get p) "stops at space: next char" show_char_opt;
  let _, res = expect_identifier (init "1abc") in
  expect_eq (Error (InvalidKeyFirstCharacter '1')) res "digit start: error" show_result;
  let _, res = expect_identifier (init "") in
  expect_eq (Error UnexpectedEof) res "empty: UnexpectedEof" show_result
;;

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () =
  run_test "init" test_init;
  run_test "peek" test_peek;
  run_test "advance" test_advance;
  run_test "advance_by" test_advance_by;
  run_test "expect_char" test_expect_char;
  run_test "expect_identifier" test_expect_identifier
;;
