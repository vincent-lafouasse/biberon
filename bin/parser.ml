[@@@warning "-69-34-37-32"]

type t =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

type error =
  | UnexpectedCharacter of char
  | UnexpectedEof
  | InvalidKeyFirstCharacter of char
  | InvalidKeyCharacter of char

let init (input : string) : t =
  match input with
  | "" -> { input; position = 0; ch = None }
  | _ -> { input; position = 1; ch = Some (String.get input 0) }
;;

let eof (parser : t) : bool = parser.position >= String.length parser.input

let get (parser : t) : char option = parser.ch

let peek (parser : t) : char option =
  if eof parser then None else Some (String.get parser.input parser.position)
;;

let advance_by parser offset : t =
  let offset = max 0 offset in
  let new_position = min (parser.position + offset) (String.length parser.input) in
  let parser = { parser with position = new_position } in
  let ch =
    if eof parser then None else Some (String.get parser.input (new_position - 1))
  in
  { parser with ch }
;;

let advance (parser : t) : t = advance_by parser 1

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

let char_is_ident : char -> bool = either Char.Ascii.is_alphanum (Char.equal '_')
let char_is_ident_start : char -> bool = either Char.Ascii.is_letter (Char.equal '_')

let find_word_end (parser : t) : t * int = parser, 4567865467876546

let expect_identifier (parser : t) : t * (string, error) result =
  let start : int = parser.position - 1 in
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
      let past_end_parser, end_position = find_word_end parser in
      past_end_parser, Ok end_position
  in
  let identifier_res =
    match end_position_res with
    | Error err -> Error err
    | Ok end_pos -> Ok (String.sub parser.input start (end_pos - start))
  in
  parser, identifier_res
;;

(* parser -> parser * Entry.raw_entry option *)
let next_raw_entry (_parser : t) = failwith "todo"

let log (parser : t) : unit = print_endline (show parser)

let expect cond msg = if not cond then failwith ("FAIL: " ^ msg)

let test_init () =
  let empty = init "" in
  expect (eof empty) "empty: eof";
  expect (get empty = None) "empty: ch=None";
  let p = init "abc" in
  expect (get p = Some 'a') "ch=Some 'a'";
  expect (not (eof p)) "not eof"
;;

let test_peek () =
  let p = init "abc" in
  expect (peek p = Some 'b') "peek first";
  expect (peek (advance (advance p)) = None) "peek at last char";
  expect (peek (init "") = None) "peek at eof"
;;

let test_advance () =
  let p = init "abc" in
  let p1 = advance p in
  expect (get p1 = Some 'b') "ch=Some 'b'";
  expect (peek p1 = Some 'c') "peek=Some 'c'";
  let p2 = advance p1 in
  expect (get p2 = Some 'c') "ch=Some 'c'";
  expect (peek p2 = None) "peek=None at end";
  let p3 = advance p2 in
  expect (eof p3) "eof";
  expect (get p3 = None) "ch=None past end";
  let p4 = advance p3 in
  expect (p3 = p4) "idempotent at eof"
;;

let test_advance_by () =
  expect (get (advance_by (init "abcde") 3) = Some 'c') "advance_by 3";
  expect (eof (advance_by (init "abc") 99)) "advance_by past end: eof";
  expect (get (advance_by (init "abc") 0) = Some 'a') "advance_by 0: no change"
;;

let test_expect_char () =
  let p, err = expect_char (init "abc") Char.Ascii.is_letter in
  expect (err = None) "match: no error";
  expect (get p = Some 'b') "match: advanced";
  let p, err = expect_char (init "abc") Char.Ascii.is_digit in
  expect (err = Some (UnexpectedCharacter 'a')) "no match: error";
  expect (get p = Some 'a') "no match: not advanced";
  let _, err = expect_char (init "") Char.Ascii.is_letter in
  expect (err = Some UnexpectedEof) "eof: UnexpectedEof"
;;

let test_expect_identifier () =
  let p, res = expect_identifier (init "abc") in
  expect (res = Ok "abc") "happy: Ok abc";
  expect (eof p) "happy: parser at end";
  let p, res = expect_identifier (init "foo_bar baz") in
  expect (res = Ok "foo_bar") "stops at space";
  expect (get p = Some ' ') "stops at space: next char";
  let _, res = expect_identifier (init "1abc") in
  expect (res = Error (InvalidKeyFirstCharacter '1')) "digit start: error";
  let _, res = expect_identifier (init "") in
  expect (res = Error UnexpectedEof) "empty: UnexpectedEof"
;;

let __test () =
  test_init ();
  test_peek ();
  test_advance ();
  test_advance_by ();
  test_expect_char ();
  test_expect_identifier ();
  print_endline "all tests passed"
;;
