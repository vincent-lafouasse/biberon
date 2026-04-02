module Value = struct
  type t =
    | Boolean of bool
    | Integer of int
    | String of string
  [@@deriving show]
end

type etype = Etype of string [@@deriving show]
type tag = Tag of string [@@deriving show]
type key = Key of string [@@deriving show]
type field = key * Value.t [@@deriving show]
type common_fields =
  { author : string
  ; title : string
  ; year : int
  ; archive : string
  }
[@@deriving show]

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
[@@deriving show]

type article_fields =
  { journal : string
  ; volume : int
  ; pages : int * int
  ; number : int
  ; month : month
  ; doi : string
  }
[@@deriving show]

type inproceedings_fields =
  { booktitle : string
  ; pages : int * int
  ; doi : string
  }
[@@deriving show]

type t =
  | Article of common_fields * article_fields
  | Inproceedings of common_fields * inproceedings_fields
  | Other of etype * common_fields
[@@deriving show]

type raw_entry =
  { etype : etype
  ; tag : tag
  ; fields : field array
  }
[@@deriving show]
