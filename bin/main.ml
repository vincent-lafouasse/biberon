[@@@warning "-69-34-37-32"]

type parser = { input : string; position : int; ch : char option }
[@@deriving show]

let parser_init (input : string) : parser option =
  match input with
  | "" -> None
  | _ -> Some { input; position = 0; ch = Some (String.get input 0) }

let parser_eof (parser : parser) : bool =
  parser.position >= String.length parser.input

let parser_advance (parser : parser) : parser =
  if parser_eof parser then parser
  else { parser with position = parser.position + 1 }

(* val parser_expect_identifier: parser -> parser * string option *)
(* val parser_expect_char: parser -> char -> parser option *)
(* val parser_next_raw_entry: parser -> parser * raw_entry option *)

let die msg =
  print_endline msg;
  exit 1

let parser_from_file_or_die (path : string) : parser =
  (* at some point i'll probably wrap this so it errs instead of throwing
   but for now i'll just assume everything went ok, i'll learn to deal with
   exceptions later *)
  let read_file (path : string) : string =
    In_channel.with_open_bin path In_channel.input_all
  in
  let input = read_file path in
  match parser_init input with None -> die "empty" | Some parser -> parser

let input = "abc"

let parser =
  match parser_init input with None -> die "empty" | Some parser -> parser

let () = print_endline (show_parser parser)
let parser = parser_advance parser
let () = print_endline (show_parser parser)
let parser = parser_advance parser
let () = print_endline (show_parser parser)
let parser = parser_advance parser
let () = print_endline (show_parser parser)
let parser = parser_advance parser
let () = print_endline (show_parser parser)
