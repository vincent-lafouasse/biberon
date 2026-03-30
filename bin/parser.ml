[@@@warning "-69-34-37-32"]

type t = { input : string; position : int; ch : char option } [@@deriving show]

let init (input : string) : t =
  match input with
  | "" -> { input; position = 0; ch = None }
  | _ -> { input; position = 0; ch = Some (String.get input 0) }

let eof (parser : t) : bool = parser.position >= String.length parser.input

(* TODO: don't forget to update ch *)
let advance (parser : t) : t =
  if eof parser then parser else { parser with position = parser.position + 1 }

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
  ()
