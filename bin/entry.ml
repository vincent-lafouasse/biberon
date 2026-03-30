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

type bib_entry =
  | Article of common_fields * article_fields
  | Inproceedings of common_fields * inproceedings_fields
  | Other of etype * common_fields

(* intermediate type used by the parser before typed conversion *)
type raw_entry = { etype : string; fields : (string * string) list }
