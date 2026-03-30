[@@@warning "-69-34-37-32"]

type t = { input : string; position : int; ch : char option } [@@deriving show]

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

(* parser -> char -> parser option *)
let expect_char (_parser : t) (_c : char) = failwith "todo"

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
