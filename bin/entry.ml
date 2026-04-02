type common_fields =
  { author : string
  ; title : string
  ; year : int
  ; archive : string
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

type article_fields =
  { journal : string
  ; volume : int
  ; pages : int * int
  ; number : int
  ; month : month
  ; doi : string
  }

type inproceedings_fields =
  { booktitle : string
  ; pages : int * int
  ; doi : string
  }

type etype = Etype of string

type bib_entry =
  | Article of common_fields * article_fields
  | Inproceedings of common_fields * inproceedings_fields
  | Other of etype * common_fields

type key = Key of string

module Value = struct
  type t =
    | Boolean of bool
    | Integer of int
    | String of string
  [@@deriving show]
end

type raw_entry =
  { etype : etype
  ; fields : (key * Value.t) list
  }
