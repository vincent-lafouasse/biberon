[@@@warning "-37"]

type style = IEEE
type target = Markdown

type modifier =
  | Normal
  | Bold
  | Italic
  | SmallCaps
[@@deriving show]

type blob =
  | Text of
      { text : string
      ; modifier : modifier
      }
  | Link of
      { display : string
      ; url : string
      }
[@@deriving show]

type t = blob list [@@deriving show]

type segment =
  | Plain of string
  | Verbatim of string
[@@deriving show]

let parse_segments (s : string) : segment list =
  let n = String.length s in
  let rec go i plain acc =
    if i >= n
    then List.rev (if plain = "" then acc else Plain plain :: acc)
    else (
      match s.[i] with
      | '\\' ->
        let escaped = if i + 1 < n then String.make 1 s.[i + 1] else "" in
        go (i + 2) (plain ^ escaped) acc
      | '{' ->
        let acc = if plain = "" then acc else Plain plain :: acc in
        let rec read_verbatim j verbatim =
          if j >= n
          then verbatim, j
          else (
            match s.[j] with
            | '}' -> verbatim, j + 1
            | c -> read_verbatim (j + 1) (verbatim ^ String.make 1 c))
        in
        let verbatim, j = read_verbatim (i + 1) "" in
        go j "" (Verbatim verbatim :: acc)
      | c -> go (i + 1) (plain ^ String.make 1 c) acc)
  in
  go 0 "" []
;;

let capitalize_word (w : string) : string =
  String.uppercase_ascii (String.sub w 0 1)
  ^ String.lowercase_ascii (String.sub w 1 (String.length w - 1))
;;

let apply_case (transform : is_first:bool -> string -> string) (segments : segment list)
  : string
  =
  let transform_plain is_first p =
    let words = String.split_on_char ' ' p in
    let rev_words, _ =
      List.fold_left
        (fun (acc, first) w ->
           if w = "" then "" :: acc, first else transform ~is_first:first w :: acc, false)
        ([], is_first)
        words
    in
    String.concat " " (List.rev rev_words)
  in
  let result, _ =
    List.fold_left
      (fun (acc, is_first) seg ->
         match seg with
         | Verbatim v -> acc ^ v, is_first
         | Plain p ->
           let has_word = List.exists (fun w -> w <> "") (String.split_on_char ' ' p) in
           acc ^ transform_plain is_first p, is_first && not has_word)
      ("", true)
      segments
  in
  result
;;

let verbatim_case (s : string) : string =
  apply_case
    (fun ~is_first w ->
       if is_first
       then
         String.uppercase_ascii (String.sub w 0 1) ^ String.sub w 1 (String.length w - 1)
       else w)
    (parse_segments s)
;;

let sentence_case (s : string) : string =
  apply_case
    (fun ~is_first w -> if is_first then capitalize_word w else String.lowercase_ascii w)
    (parse_segments s)
;;

let non_capitalised_in_title =
  [ "a"
  ; "an"
  ; "the"
  ; "and"
  ; "but"
  ; "or"
  ; "nor"
  ; "for"
  ; "so"
  ; "yet"
  ; "as"
  ; "at"
  ; "by"
  ; "in"
  ; "of"
  ; "on"
  ; "to"
  ; "up"
  ; "via"
  ; "with"
  ]
;;

let title_case (s : string) : string =
  apply_case
    (fun ~is_first w ->
       if is_first
       then capitalize_word w
       else if List.mem (String.lowercase_ascii w) non_capitalised_in_title
       then String.lowercase_ascii w
       else capitalize_word w)
    (parse_segments s)
;;

let ieee_format_author (author : Entry.author) : string =
  let initials = List.map (fun name -> String.sub name 0 1 ^ ".") author.first in
  String.concat " " initials ^ " " ^ author.last
;;

let txt s = Text { text = s; modifier = Normal }
let italic s = Text { text = s; modifier = Italic }

let ieee_format_month (m : Entry.month) : string =
  match m with
  | Entry.Jan -> "Jan."
  | Entry.Feb -> "Feb."
  | Entry.Mar -> "Mar."
  | Entry.Apr -> "Apr."
  | Entry.May -> "May"
  | Entry.Jun -> "Jun."
  | Entry.Jul -> "Jul."
  | Entry.Aug -> "Aug."
  | Entry.Sep -> "Sep."
  | Entry.Oct -> "Oct."
  | Entry.Nov -> "Nov."
  | Entry.Dec -> "Dec."
;;

let ieee_format_doi (doi : Entry.doi) : blob =
  let display = doi.prefix ^ "/" ^ doi.suffix in
  Link { display; url = "https://doi.org/" ^ display }
;;

let ieee_format_author_list (author_list : Entry.author list) : blob =
  let text =
    match author_list with
    | [] -> ""
    | _ when List.length author_list >= 7 ->
      ieee_format_author (List.hd author_list) ^ " et al."
    | _ ->
      let formatted = List.map ieee_format_author author_list in
      (match formatted with
       | [] -> ""
       | [ single ] -> single
       | _ ->
         let all_but_last =
           List.filteri (fun i _ -> i < List.length formatted - 1) formatted
         in
         let last = List.nth formatted (List.length formatted - 1) in
         String.concat ", " all_but_last ^ ", and " ^ last)
  in
  Text { text; modifier = Normal }
;;

let ieee_format_other (etype : Entry.etype) (common : Entry.common_fields) : t =
  let (Entry.Etype etype_str) = etype in
  [ ieee_format_author_list common.author
  ; txt (Printf.sprintf ", \"%s\", " (verbatim_case common.title))
  ; txt (Printf.sprintf "%s, %d, " etype_str common.year)
  ; Link { display = "Archive"; url = common.archive }
  ; txt "."
  ]
;;

let ieee_format_inproceedings
      (common : Entry.common_fields)
      (fields : Entry.inproceedings_fields)
  : t
  =
  let start_page, end_page = fields.pages in
  [ ieee_format_author_list common.author
  ; txt (Printf.sprintf ", \"%s\", in " (sentence_case common.title))
  ; italic (title_case fields.booktitle)
  ; txt (Printf.sprintf ", %d, pp. %s--%s, doi: " common.year start_page end_page)
  ; ieee_format_doi fields.doi
  ; txt ", "
  ; Link { display = "Archive"; url = common.archive }
  ; txt "."
  ]
;;

let ieee_format_article (common : Entry.common_fields) (fields : Entry.article_fields) : t
  =
  let start_page, end_page = fields.pages in
  [ ieee_format_author_list common.author
  ; txt (Printf.sprintf ", \"%s\", " (sentence_case common.title))
  ; italic (title_case fields.journal)
  ; txt
      (Printf.sprintf
         ", vol. %d, no. %d, pp. %s--%s, %s %d, doi: "
         fields.volume
         fields.number
         start_page
         end_page
         (ieee_format_month fields.month)
         common.year)
  ; ieee_format_doi fields.doi
  ; txt ", "
  ; Link { display = "Archive"; url = common.archive }
  ; txt "."
  ]
;;

let ieee_format (entry : Entry.t) : t =
  match entry with
  | Article (common, fields) -> ieee_format_article common fields
  | Inproceedings (common, fields) -> ieee_format_inproceedings common fields
  | Other (etype, common) -> ieee_format_other etype common
;;

let with_style (entry : Entry.t) (style : style) : t =
  match style with
  | IEEE -> ieee_format entry
;;

let markdown_formatter (blob : blob) : string =
  match blob with
  | Text { text; modifier = Normal } -> text
  | Text { text; modifier = Bold } -> "**" ^ text ^ "**"
  | Text { text; modifier = Italic } -> "*" ^ text ^ "*"
  | Text { text; modifier = SmallCaps } -> text
  | Link { display; url } -> "[" ^ display ^ "](" ^ url ^ ")"
;;

let apply_formatter (formatter : blob -> string) (citation : t) : string =
  String.concat "" (List.map formatter citation)
;;

let format (citation : t) (target : target) : string =
  match target with
  | Markdown -> apply_formatter markdown_formatter citation
;;
