module Value = struct
  type t =
    | Boolean of bool
    | Integer of int
    | String of string
  [@@deriving show]

  type kind =
    | KBoolean
    | KInteger
    | KString
  [@@deriving show]

  let kind_of (value : t) : kind =
    match value with
    | Boolean _ -> KBoolean
    | Integer _ -> KInteger
    | String _ -> KString
  ;;
end

type etype = Etype of string [@@deriving show]
type tag = Tag of string [@@deriving show]
type key = Key of string [@@deriving show]
type field = key * Value.t [@@deriving show]
type doi =
  { prefix : string
  ; suffix : string
  }
[@@deriving show]

type author =
  { last : string
  ; first : string list
  }
[@@deriving show]

type common_fields =
  { author : author list
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
  ; pages : string * string
  ; number : int
  ; month : month
  ; doi : doi
  }
[@@deriving show]

type inproceedings_fields =
  { booktitle : string
  ; pages : string * string
  ; doi : doi
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
