[@@@warning "-69-34-37-32"]

type t = { input : string; position : int; ch : char option } [@@deriving show]
type error = UnexpectedCharacter of char | UnexpectedEof

let init (input : string) : t =
  match input with
  | "" -> { input; position = 0; ch = None }
  | _ -> { input; position = 1; ch = Some (String.get input 0) }

let eof (parser : t) : bool = parser.position >= String.length parser.input
let get (parser : t) : char option = parser.ch

let peek (parser : t) : char option =
  if eof parser then None else Some (String.get parser.input parser.position)

let advance (parser : t) : t =
  let position = if eof parser then parser.position else parser.position + 1 in
  let ch = peek parser in
  { parser with position; ch }

let either f g x = f x || g x
let char_is_ident = either Char.Ascii.is_alphanum (Char.equal '_')

(* parser -> parser * string *)
let expect_identifier (_parser : t) = failwith "todo"

let expect_char (parser : t) (pred : char -> bool) : t * error option =
  match get parser with
  | Some inner when pred inner -> (advance parser, None)
  | Some inner -> (parser, Some (UnexpectedCharacter inner))
  | None -> (parser, Some UnexpectedEof)

let expect_char_eq (parser : t) (c : char) : t * error option =
  expect_char parser (Char.equal c)

(* parser -> parser * Entry.raw_entry option *)
let next_raw_entry (_parser : t) = failwith "todo"
let log (parser : t) : unit = print_endline (show parser)

let __test () =
  let input = "abc" in
  let parser = init input in
  let _ = log parser in
  let parser = advance parser in
  let _ = log parser in
  let parser = advance parser in
  let _ = log parser in
  let parser = advance parser in
  let _ = log parser in
  let parser = advance parser in
  let _ = log parser in
  ()
