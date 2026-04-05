type t =
  { input : string
  ; position : Position.t
  }
[@@deriving show]

type error =
  | UnterminatedString
  | UnrecognizedCharacter of char
[@@deriving show]

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

let skip_whitespace = advance_while Char.Ascii.is_white

let fn_not f x = not (f x)

let substring lexer (start_pos : Position.t) (end_pos : Position.t) =
  String.sub lexer.input start_pos.absolute (Position.distance start_pos end_pos)
;;

let scan_while (lexer : t) (pred : char -> bool) : t * string =
  let start_pos = lexer.position in
  let past_end_lexer = advance_while pred lexer in
  let end_pos = past_end_lexer.position in
  past_end_lexer, substring lexer start_pos end_pos
;;

let scan_identifier_or_bool (lexer : t)
  : t * (Token.t Position.located, error Position.located) result
  =
  let past_end_lexer, str =
    scan_while lexer (either Char.Ascii.is_alphanum (Char.equal '_'))
  in
  let token =
    match str with
    | "true" -> Token.Value (Token.Value.Boolean true)
    | "false" -> Token.Value (Token.Value.Boolean false)
    | s -> Token.Identifier s
  in
  past_end_lexer, Ok (token, lexer.position)
;;

let scan_string (lexer : t)
  : t * (Token.t Position.located, error Position.located) result
  =
  let _ =
    if eof lexer || not (Char.equal (get_unsafe lexer) '"')
    then failwith "unreachable: string not starting with quote"
  in
  let start_lexer = lexer in
  let past_opening_quote_lexer = advance start_lexer in
  let on_closing_quote_lexer, str =
    scan_while past_opening_quote_lexer (fn_not (Char.equal '"'))
  in
  match get on_closing_quote_lexer with
  | None -> start_lexer, Error (UnterminatedString, start_lexer.position)
  | Some c when c = '"' ->
    ( advance on_closing_quote_lexer
    , Ok (Token.Value (Token.Value.String str), start_lexer.position) )
  | _ -> failwith "unreachable: scan_while didn't land on a quote or EOF"
;;

let scan_int (lexer : t) : t * (Token.t Position.located, error Position.located) result =
  let start_pos = lexer.position in
  let past_end_lexer, str = scan_while lexer Char.Ascii.is_digit in
  past_end_lexer, Ok (Token.Value (Token.Value.Integer (int_of_string str)), start_pos)
;;

let next_token (lexer : t) : t * (Token.t Position.located, error Position.located) result
  =
  let lexer = skip_whitespace lexer in
  match get lexer with
  | None -> lexer, Ok (Token.Eof, lexer.position)
  | Some c ->
    (match c with
     | '@' -> advance lexer, Ok (Token.AtSign, lexer.position)
     | '{' -> advance lexer, Ok (Token.Lbrace, lexer.position)
     | '}' -> advance lexer, Ok (Token.Rbrace, lexer.position)
     | '=' -> advance lexer, Ok (Token.EqualSign, lexer.position)
     | ',' -> advance lexer, Ok (Token.Comma, lexer.position)
     | '"' -> scan_string lexer
     | c when Char.Ascii.is_digit c -> scan_int lexer
     | c when char_is_ident_start c -> scan_identifier_or_bool lexer
     | c -> lexer, Error (UnrecognizedCharacter c, lexer.position))
;;

(* main export: *)
let tokenize input =
  let rec loop lexer acc =
    let lexer, res = next_token lexer in
    match res with
    | Error e -> Error e
    | Ok (Token.Eof, pos) -> Ok (Array.of_list (List.rev ((Token.Eof, pos) :: acc)))
    | Ok tok -> loop lexer (tok :: acc)
  in
  loop (init input) []
;;

let format_error (err : error) : string =
  match err with
  | UnterminatedString -> "Unterminated string"
  | UnrecognizedCharacter c ->
    let char_repr : string =
      if Char.Ascii.is_print c then Printf.sprintf "%c" c else Char.escaped c
    in
    Printf.sprintf "Unrecognized character %s" char_repr
;;

let format_located_error (err : error Position.located) (input : string) : string =
  let err, loc = err in
  Position.report_error loc input (format_error err)
;;

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

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let show_token_result r =
  [%show: (Token.t Position.located, error Position.located) result] r
;;

let tok input = snd (next_token (init input))

let pos absolute line column : Position.t = { absolute; line; column }

let test_next_token_structural () =
  expect_eq (Ok (Token.AtSign, pos 0 1 0)) (tok "@") "atsign" show_token_result;
  expect_eq (Ok (Token.Lbrace, pos 0 1 0)) (tok "{") "lbrace" show_token_result;
  expect_eq (Ok (Token.Rbrace, pos 0 1 0)) (tok "}") "rbrace" show_token_result;
  expect_eq (Ok (Token.EqualSign, pos 0 1 0)) (tok "=") "equalsign" show_token_result;
  expect_eq (Ok (Token.Eof, pos 0 1 0)) (tok "") "eof on empty" show_token_result
;;

let test_next_token_whitespace () =
  expect_eq (Ok (Token.AtSign, pos 3 1 3)) (tok "   @") "skips spaces" show_token_result;
  expect_eq
    (Ok (Token.Lbrace, pos 1 2 0))
    (tok "\n{")
    "skips newline, tracks line"
    show_token_result
;;

let test_next_token_identifier () =
  expect_eq
    (Ok (Token.Identifier "article", pos 0 1 0))
    (tok "article")
    "simple ident"
    show_token_result;
  expect_eq
    (Ok (Token.Identifier "foo_bar", pos 0 1 0))
    (tok "foo_bar")
    "ident with underscore"
    show_token_result;
  expect_eq
    (Ok (Token.Identifier "a123", pos 0 1 0))
    (tok "a123")
    "ident with digits"
    show_token_result
;;

let test_next_token_string () =
  expect_eq
    (Ok (Token.Value (Token.Value.String "hello"), pos 0 1 0))
    (tok {|"hello"|})
    "string"
    show_token_result;
  expect_eq
    (Ok (Token.Value (Token.Value.String ""), pos 0 1 0))
    (tok {|""|})
    "empty string"
    show_token_result;
  expect_eq
    (Ok (Token.Value (Token.Value.String "hello world"), pos 0 1 0))
    (tok {|"hello world"|})
    "string with space"
    show_token_result
;;

let test_next_token_integer () =
  expect_eq
    (Ok (Token.Value (Token.Value.Integer 42), pos 0 1 0))
    (tok "42")
    "integer"
    show_token_result;
  expect_eq
    (Ok (Token.Value (Token.Value.Integer 0), pos 0 1 0))
    (tok "0")
    "zero"
    show_token_result;
  expect_eq
    (Ok (Token.Value (Token.Value.Integer 2024), pos 0 1 0))
    (tok "2024")
    "year"
    show_token_result
;;

let test_next_token_sequence () =
  (* tokenize "@article" as two tokens *)
  let l = init "@article" in
  let l, t1 = next_token l in
  let _l, t2 = next_token l in
  expect_eq (Ok (Token.AtSign, pos 0 1 0)) t1 "seq: first token" show_token_result;
  expect_eq
    (Ok (Token.Identifier "article", pos 1 1 1))
    t2
    "seq: second token"
    show_token_result
;;

let test_next_token_error () =
  let _, res = next_token (init {|"unterminated|}) in
  expect (Result.is_error res) "unterminated string is error";
  let _, res = next_token (init "?") in
  expect (Result.is_error res) "unrecognized: ?";
  let _, res = next_token (init "$") in
  expect (Result.is_error res) "unrecognized: $";
  let _, res = next_token (init "!") in
  expect (Result.is_error res) "unrecognized: !"
;;

let test_bib_realistic () =
  let article =
    {|
@article{doe2024,
  author = "Doe, J.",
  title = "A Survey of Things",
  year = 2024,
  journal = "Journal of Things",
  volume = 12,
  number = 3,
  pages = "1--10",
  month = "Jun",
  doi = "10.1234/jot.2024",
  archive = "https://arxiv.org/abs/2024.00001"
}
|}
  in
  let res = tokenize article in
  expect (Result.is_ok res) "article: no lex errors";
  (* spot check a few tokens in the stream *)
  (match res with
   | Error _ -> ()
   | Ok tokens ->
     let has tok = Array.exists (fun (t, _) -> t = tok) tokens in
     expect (has Token.AtSign) "article: contains @";
     expect (has (Token.Identifier "article")) "article: contains ident 'article'";
     expect (has (Token.Identifier "doe2024")) "article: contains key 'doe2024'";
     expect (has Token.Lbrace) "article: contains {";
     expect (has Token.Rbrace) "article: contains }");
  let inproceedings =
    {|
@inproceedings{smith2023,
  author = "Smith, A. and Jones, B.",
  title = "Fast Algorithms",
  year = 2023,
  booktitle = "Proceedings of the Conference on Speed",
  pages = "42--55",
  doi = "10.5678/conf.2023",
  archive = "https://arxiv.org/abs/2023.99999"
}
|}
  in
  let res = tokenize inproceedings in
  expect (Result.is_ok res) "inproceedings: no lex errors";
  let misc =
    {|
@misc{blog2022,
  author = "A. Blogger",
  title = "Some Thoughts",
  year = 2022,
  archive = "https://example.com/post"
}
|}
  in
  let res = tokenize misc in
  expect (Result.is_ok res) "misc: no lex errors"
;;

let __test () =
  run_test "init" test_init;
  run_test "peek" test_peek;
  run_test "advance" test_advance;
  run_test "advance_by" test_advance_by;
  run_test "next_token: structural" test_next_token_structural;
  run_test "next_token: whitespace" test_next_token_whitespace;
  run_test "next_token: identifier" test_next_token_identifier;
  run_test "next_token: string" test_next_token_string;
  run_test "next_token: integer" test_next_token_integer;
  run_test "next_token: sequence" test_next_token_sequence;
  run_test "next_token: error" test_next_token_error;
  run_test "next_token: realistic bib" test_bib_realistic
;;
