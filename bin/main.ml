[@@@warning "-69-34-37-32"]

type common_fields = {
  author : string;
  title : string;
  year : int;
  archive : string;
}

type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

type article_fields = {
  journal : string;
  volume : int;
  pages : int * int;
  number : int;
  month : month;
  doi : string;
}

type inproceedings_fields = {
  booktitle : string;
  pages : int * int;
  doi : string;
}

type etype = Etype of string

(* fallback to Other if any etype is unrecognised.
   they should still have the common fields
*)
(* invalid entries should not be representable *)
type bib_entry =
  | Article of common_fields * article_fields
  | Inproceedings of common_fields * inproceedings_fields
  | Other of etype * common_fields

type raw_entry = { etype : string; fields : (string * string) list }

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

let parser =
  let path =
    match Array.length Sys.argv with
    | 2 -> Sys.argv.(1)
    | _ -> die "Usage: biberon refs.bib"
  in
  parser_from_file_or_die path

let () = print_endline (show_parser parser)
