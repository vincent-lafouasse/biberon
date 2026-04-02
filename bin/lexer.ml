[@@@warning "-69-34-37-32"]

type t =
  { input : string
  ; position : Position.t
  }
[@@deriving show]

type error = UnterminatedString [@@deriving show]

let init (input : string) : t =
  let position : Position.t = { absolute = 0; line = 1; column = 0 } in
  { input; position }
;;

let len lexer = String.length lexer.input

let at lexer (position : Position.t) = String.get lexer.input position.absolute

let eof lexer = lexer.position.absolute >= len lexer

let get (lexer : t) : char option =
  if eof lexer then None else Some (at lexer lexer.position)
;;

let get_unsafe lexer : char = Option.get (get lexer)

let substring lexer (start_pos : Position.t) (end_pos : Position.t) =
  String.sub lexer.input start_pos.absolute (Position.distance start_pos end_pos)
;;

let increment_position (position : Position.t) break_line : Position.t =
  let absolute = position.absolute + 1 in
  let line, column =
    if break_line then position.line + 1, 0 else position.line, position.column + 1
  in
  { absolute; line; column }
;;

let advance lexer =
  let can_advance = not (eof lexer) in
  let break_line = can_advance && Char.equal '\n' (get_unsafe lexer) in
  let position = lexer.position in
  let position =
    if can_advance then increment_position position break_line else position
  in
  { lexer with position }
;;

let peek (lexer : t) : char option =
  let lexer = advance lexer in
  get lexer
;;

let rec advance_by lexer offset : t =
  if offset > 0 then advance_by (advance lexer) (offset - 1) else lexer
;;

let either f g x = f x || g x

let char_is_ident_start : char -> bool = either Char.Ascii.is_letter (Char.equal '_')

let rec advance_while pred lexer =
  let continue = (not (eof lexer)) && pred (get_unsafe lexer) in
  if continue then advance_while pred (advance lexer) else lexer
;;

let find_word_end = advance_while (either Char.Ascii.is_alphanum (Char.equal '_'))

let expect_identifier (lexer : t) : t * (string, error) result =
  let start : Position.t = lexer.position in
  let lexer, first_char_res =
    match get lexer with
    | None -> lexer, Error UnexpectedEof
    | Some c when char_is_ident_start c -> advance lexer, Ok ()
    | Some c -> lexer, Error (InvalidKeyFirstCharacter c)
  in
  let lexer, end_position_res =
    match first_char_res with
    | Error err -> lexer, Error err
    | Ok _ ->
      let past_end_lexer = find_word_end lexer in
      past_end_lexer, Ok past_end_lexer.position
  in
  let identifier_res =
    match end_position_res with
    | Error err -> Error err
    | Ok end_pos -> Ok (substring lexer start end_pos)
  in
  lexer, identifier_res
;;

let skip_whitespace = advance_while Char.Ascii.is_white

let fn_not f x = not (f x)

(* main export: *)
let next_token (_lexer : t) : t * (Token.t, error) result = failwith "todo"

let log (lexer : t) : unit = print_endline (show lexer)

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

let test_expect_identifier () =
  let p, res = expect_identifier (init "abc") in
  expect_eq (Ok "abc") res "happy: Ok abc" show_result;
  expect (eof p) "happy: lexer at end";
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
  run_test "expect_identifier" test_expect_identifier
;;
