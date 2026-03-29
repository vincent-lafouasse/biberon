[@@@warning "-69-34-37"]

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

type etype = string

(* fallback to Other if any etype is unrecognised. they should still have the common fields *)
(* invalid entries should not be representable *)
type bib_entry =
  | Article of common_fields * article_fields
  | Inproceedings of common_fields * inproceedings_fields
  | Other of etype * common_fields

type raw_entry = { etype : string; fields : (string * string) list }
type parser = { input : string; position : int; ch : char option }

(* input -> parser option *)
let init_parser input =
  match input with
  | "" -> None
  | _ -> Some { input; position = 0; ch = Some (String.get input 0) }

let foo =
  { author = "hello"; title = "world"; year = 67; archive = "google.com" }

let () = print_endline foo.author
