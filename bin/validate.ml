type 'a expected = Expected of 'a [@@deriving show]
type 'a actual = Actual of 'a [@@deriving show]

type error =
  | DuplicateEntry of Entry.tag
  | DuplicateField of Entry.key * Entry.tag
  | ValueTypeMismatch of
      Entry.key * Entry.tag * Entry.Value.kind expected * Entry.Value.kind actual
  | MissingField of Entry.key * Entry.tag
  | MalformedAuthorName of string * Entry.tag
  | MalformedDoi of string * Entry.tag
  | MalformedMonth of string * Entry.tag
  | MalformedPageRange of string * Entry.tag
[@@deriving show]

module StringMap = Map.Make (String)

(* responsibilities:
    - recognize the etype
    - assert existence and unicity of common and specific fields
    - assert that those fields are not malformed
    - package them in a finished entry
*)

(* checking for duplicate entries (resp. duplicate fields) and making
   duplication fatal makes it so that past the duplication detection, errors
   can use entry tag (resp. field key) to refer to the parent entry
   unambiguously *)

(* this will error the same way whether there are 1 or multiple duplications
   i might return a list of duplicates instead but for now that's fine *)
let find_duplicates (key_of : 'a -> string) (collection : 'a array) : string option =
  let update_count map (e : 'a) =
    let key = key_of e in
    let count =
      match StringMap.find_opt key map with
      | None -> 1
      | Some count -> count + 1
    in
    StringMap.add key count map
  in
  let frequency_map = Array.fold_left update_count StringMap.empty collection in
  let is_duplicate : string -> int -> bool = fun _tag count -> count > 1 in
  let duplicates = StringMap.filter is_duplicate frequency_map in
  match StringMap.choose_opt duplicates with
  | None -> None
  | Some (tag, _count) -> Some tag
;;

let assert_no_duplicate_entry (raw_lib : Entry.raw_entry array) : (unit, error) result =
  let tag_of (entry : Entry.raw_entry) : string =
    let (Entry.Tag tag) = entry.tag in
    tag
  in
  let maybe_duplicate : string option = find_duplicates tag_of raw_lib in
  match maybe_duplicate with
  | None -> Ok ()
  | Some tag -> Error (DuplicateEntry (Entry.Tag tag))
;;

let assert_no_duplicate_field (raw_entry : Entry.raw_entry) : (unit, error) result =
  let field_name (field : Entry.field) : string =
    let key, _value = field in
    let (Entry.Key key_name) = key in
    key_name
  in
  let maybe_duplicate : string option = find_duplicates field_name raw_entry.fields in
  match maybe_duplicate with
  | None -> Ok ()
  | Some key_name -> Error (DuplicateField (Entry.Key key_name, raw_entry.tag))
;;

let locate_field (raw_entry : Entry.raw_entry) (field_name : string)
  : Entry.Value.t option
  =
  let maybe_field : Entry.field option =
    Array.find_opt
      (fun ((key, _value) : Entry.field) -> key = Entry.Key field_name)
      raw_entry.fields
  in
  Option.map (fun (_key, value) -> value) maybe_field
;;

(* probably unsafe, just be careful. should only be called after type check *)
let unwrap_string (value : Entry.Value.t) : string =
  match value with
  | Entry.Value.String str -> str
  | _ -> failwith "unwrap_string on non string value"
;;

let unwrap_int (value : Entry.Value.t) : int =
  match value with
  | Entry.Value.Integer i -> i
  | _ -> failwith "unwrap_int on non int value"
;;

let get_string_field (entry : Entry.raw_entry) (key : Entry.key) : (string, error) result =
  let (Entry.Key key_name) = key in
  let maybe_value : Entry.Value.t option = locate_field entry key_name in
  let value_res : (Entry.Value.t, error) result =
    Option.to_result ~none:(MissingField (key, entry.tag)) maybe_value
  in
  let unwrap_string_or_err (value : Entry.Value.t) : (string, error) result =
    if Entry.Value.kind_of value = Entry.Value.KString
    then Ok (unwrap_string value)
    else (
      let expected = Expected Entry.Value.KString in
      let actual = Actual (Entry.Value.kind_of value) in
      Error (ValueTypeMismatch (key, entry.tag, expected, actual)))
  in
  Result.bind value_res unwrap_string_or_err
;;

let get_int_field (entry : Entry.raw_entry) (key : Entry.key) : (int, error) result =
  let (Entry.Key key_name) = key in
  let maybe_value : Entry.Value.t option = locate_field entry key_name in
  let value_res : (Entry.Value.t, error) result =
    Option.to_result ~none:(MissingField (key, entry.tag)) maybe_value
  in
  let unwrap_int_or_err (value : Entry.Value.t) : (int, error) result =
    if Entry.Value.kind_of value = Entry.Value.KInteger
    then Ok (unwrap_int value)
    else (
      let expected = Expected Entry.Value.KInteger in
      let actual = Actual (Entry.Value.kind_of value) in
      Error (ValueTypeMismatch (key, entry.tag, expected, actual)))
  in
  Result.bind value_res unwrap_int_or_err
;;

(* error payload is its own raw input, this way i can keep track of which
   author is malformed. the error will be wrapped in a Validate.error at the
   call site *)
(* returning `MalformedAuthorName of string * tag` means we have to be in a
   scope where we know the entry tag *)
let parse_single_author author_str : (Entry.author, string) result =
  let last_first : string list = Str.split (Str.regexp ",") author_str in
  let last_first : (string * string, string) result =
    match last_first with
    | [ last; first ] -> Ok (String.trim last, String.trim first)
    | _ -> Error (String.trim author_str)
  in
  let split_first_names (last_first : string * string) : Entry.author =
    let last, first = last_first in
    let first : string list = Str.split (Str.regexp " ") first in
    let first : string list = List.map String.trim first in
    { Entry.last; first }
  in
  Result.map split_first_names last_first
;;

let parse_author_list (author_list_str : string) : (Entry.author list, string) result =
  let raw_author_list = Str.split (Str.regexp "and") author_list_str in
  let author_res_list = List.map parse_single_author raw_author_list in
  match List.find_opt Result.is_error author_res_list with
  | Some bad_name_error ->
    let bad_name = Result.get_error bad_name_error in
    Error bad_name
  | None -> Ok (List.map Result.get_ok author_res_list)
;;

let parse_author_list_wrapped (tag : Entry.tag) (author_list_str : string)
  : (Entry.author list, error) result
  =
  Result.map_error
    (fun bad_name -> MalformedAuthorName (bad_name, tag))
    (parse_author_list author_list_str)
;;

let get_common_fields (entry : Entry.raw_entry) : (Entry.common_fields, error) result =
  let ( let* ) = Result.bind in
  let* author_str = get_string_field entry (Entry.Key "author") in
  let* author = parse_author_list_wrapped entry.tag author_str in
  let* title = get_string_field entry (Entry.Key "title") in
  let* year = get_int_field entry (Entry.Key "year") in
  let* archive = get_string_field entry (Entry.Key "archive") in
  Ok { Entry.author; title; year; archive }
;;

(* same as with the authors, error payload is the fautive string. wrapping at
   callsite *)
let parse_month (month : string) : (Entry.month, string) result =
  match month with
  | "jan" -> Ok Entry.Jan
  | "feb" -> Ok Entry.Feb
  | "mar" -> Ok Entry.Mar
  | "apr" -> Ok Entry.Apr
  | "may" -> Ok Entry.May
  | "jun" -> Ok Entry.Jun
  | "jul" -> Ok Entry.Jul
  | "aug" -> Ok Entry.Aug
  | "sep" -> Ok Entry.Sep
  | "oct" -> Ok Entry.Oct
  | "nov" -> Ok Entry.Nov
  | "dec" -> Ok Entry.Dec
  | bad_month -> Error bad_month
;;

let parse_page_range (range : string) : (string * string, string) result =
  match Str.split (Str.regexp "--") range with
  | [ start_page; end_page ] -> Ok (String.trim start_page, String.trim end_page)
  | _ -> Error range
;;

let parse_doi (raw : string) : (Entry.doi, string) result =
  let strip_prefix p s =
    if String.starts_with ~prefix:p s
    then String.sub s (String.length p) (String.length s - String.length p)
    else s
  in
  let strip_suffix suf s =
    if String.ends_with ~suffix:suf s
    then String.sub s 0 (String.length s - String.length suf)
    else s
  in
  let stripped =
    raw
    |> strip_prefix "https://doi.org/"
    |> strip_prefix "http://doi.org/"
    |> strip_suffix "/"
  in
  if not (String.starts_with ~prefix:"10." stripped)
  then Error raw
  else (
    match String.index_opt stripped '/' with
    | None -> Error raw
    | Some i ->
      let prefix = String.sub stripped 0 i in
      let suffix = String.sub stripped (i + 1) (String.length stripped - i - 1) in
      if String.length suffix = 0 then Error raw else Ok { Entry.prefix; suffix })
;;

let parse_month_wrapped (tag : Entry.tag) (month : string) : (Entry.month, error) result =
  Result.map_error (fun _ -> MalformedMonth (month, tag)) (parse_month month)
;;

let parse_doi_wrapped (tag : Entry.tag) (doi : string) : (Entry.doi, error) result =
  Result.map_error (fun _ -> MalformedDoi (doi, tag)) (parse_doi doi)
;;

let parse_page_range_wrapped (tag : Entry.tag) (page_range : string)
  : (string * string, error) result
  =
  Result.map_error
    (fun _ -> MalformedPageRange (page_range, tag))
    (parse_page_range page_range)
;;

let get_inproceedings_fields (entry : Entry.raw_entry)
  : (Entry.inproceedings_fields, error) result
  =
  let ( let* ) = Result.bind in
  let* booktitle = get_string_field entry (Entry.Key "booktitle") in
  let* pages_str = get_string_field entry (Entry.Key "pages") in
  let* pages = parse_page_range_wrapped entry.tag pages_str in
  let* doi_str = get_string_field entry (Entry.Key "doi") in
  let* doi = parse_doi_wrapped entry.tag doi_str in
  Ok { Entry.booktitle; pages; doi }
;;

let get_article_fields (entry : Entry.raw_entry) : (Entry.article_fields, error) result =
  let ( let* ) = Result.bind in
  let* journal = get_string_field entry (Entry.Key "journal") in
  let* volume = get_int_field entry (Entry.Key "volume") in
  let* pages_str = get_string_field entry (Entry.Key "pages") in
  let* pages = parse_page_range_wrapped entry.tag pages_str in
  let* number = get_int_field entry (Entry.Key "number") in
  let* month_str = get_string_field entry (Entry.Key "month") in
  let* month = parse_month_wrapped entry.tag month_str in
  let* doi_str = get_string_field entry (Entry.Key "doi") in
  let* doi = parse_doi_wrapped entry.tag doi_str in
  Ok { Entry.journal; volume; pages; number; month; doi }
;;

let validate_entry (entry : Entry.raw_entry) : (Entry.tag * Entry.t, error) result =
  let _ = entry in
  failwith "todo"
;;

(* ----------tests---------- *)

let expect cond msg = if not cond then failwith ("FAIL: " ^ msg)

let expect_eq expected actual msg show =
  if expected <> actual
  then (
    Printf.eprintf
      "FAIL: %s\n  expected: %s\n  got:      %s\n"
      msg
      (show expected)
      (show actual);
    exit 1)
;;

let show_result r = [%show: (unit, error) result] r

let make_entry tag : Entry.raw_entry =
  { etype = Entry.Etype "misc"; tag = Entry.Tag tag; fields = [||] }
;;

let test_assert_no_duplicate_entry () =
  (* empty library *)
  expect_eq (Ok ()) (assert_no_duplicate_entry [||]) "empty: no duplicates" show_result;
  (* single entry *)
  expect_eq
    (Ok ())
    (assert_no_duplicate_entry [| make_entry "a" |])
    "single entry: no duplicates"
    show_result;
  (* multiple distinct entries *)
  expect_eq
    (Ok ())
    (assert_no_duplicate_entry [| make_entry "a"; make_entry "b"; make_entry "c" |])
    "distinct entries: no duplicates"
    show_result;
  (* single duplication *)
  expect_eq
    (Error (DuplicateEntry (Entry.Tag "a")))
    (assert_no_duplicate_entry [| make_entry "a"; make_entry "a" |])
    "single duplication: reports duplicate tag"
    show_result;
  (* duplicate is not the only entry *)
  expect_eq
    (Error (DuplicateEntry (Entry.Tag "b")))
    (assert_no_duplicate_entry [| make_entry "a"; make_entry "b"; make_entry "b" |])
    "duplicate among others: reports duplicate tag"
    show_result;
  (* duplicate appears first *)
  expect_eq
    (Error (DuplicateEntry (Entry.Tag "a")))
    (assert_no_duplicate_entry [| make_entry "a"; make_entry "b"; make_entry "a" |])
    "duplicate appears first: reports correct tag"
    show_result
;;

let make_entry_with_fields tag fields : Entry.raw_entry =
  { etype = Entry.Etype "misc"; tag = Entry.Tag tag; fields }
;;

let field k v : Entry.field = Entry.Key k, Entry.Value.String v
let int_field k v : Entry.field = Entry.Key k, Entry.Value.Integer v

let show_field_result r = [%show: (unit, error) result] r
let show_value_opt r = [%show: Entry.Value.t option] r

let test_assert_no_duplicate_field () =
  let no_fields = make_entry_with_fields "e" [||] in
  expect_eq
    (Ok ())
    (assert_no_duplicate_field no_fields)
    "no fields: ok"
    show_field_result;
  let distinct = make_entry_with_fields "e" [| field "a" "1"; field "b" "2" |] in
  expect_eq
    (Ok ())
    (assert_no_duplicate_field distinct)
    "distinct fields: ok"
    show_field_result;
  let dup = make_entry_with_fields "e" [| field "a" "1"; field "a" "2" |] in
  expect_eq
    (Error (DuplicateField (Entry.Key "a", Entry.Tag "e")))
    (assert_no_duplicate_field dup)
    "duplicate field: reports key and tag"
    show_field_result;
  let dup_not_first =
    make_entry_with_fields "e" [| field "a" "1"; field "b" "2"; field "b" "3" |]
  in
  expect_eq
    (Error (DuplicateField (Entry.Key "b", Entry.Tag "e")))
    (assert_no_duplicate_field dup_not_first)
    "duplicate not first: reports correct key"
    show_field_result
;;

let test_locate_field () =
  let entry =
    make_entry_with_fields "e" [| field "author" "Doe"; field "year" "2024" |]
  in
  expect_eq
    (Some (Entry.Value.String "Doe"))
    (locate_field entry "author")
    "found: author"
    show_value_opt;
  expect_eq
    (Some (Entry.Value.String "2024"))
    (locate_field entry "year")
    "found: year"
    show_value_opt;
  expect_eq None (locate_field entry "title") "not found: title" show_value_opt;
  expect_eq
    None
    (locate_field (make_entry "empty") "author")
    "empty entry: not found"
    show_value_opt
;;

let show_author_list_result r = [%show: (Entry.author list, string) result] r

let test_parse_author_list () =
  expect_eq
    (Ok [ { Entry.last = "Doe"; first = [ "J." ] } ])
    (parse_author_list "Doe, J.")
    "single author comma format"
    show_author_list_result;
  expect_eq
    (Ok [ { Entry.last = "Doe"; first = [ "John"; "A." ] } ])
    (parse_author_list "Doe, John A.")
    "single author multiple first names"
    show_author_list_result;
  expect_eq
    (Ok
       [ { Entry.last = "Doe"; first = [ "J." ] }
       ; { Entry.last = "Smith"; first = [ "A." ] }
       ])
    (parse_author_list "Doe, J. and Smith, A.")
    "two authors"
    show_author_list_result;
  expect_eq
    (Ok
       [ { Entry.last = "Doe"; first = [ "J." ] }
       ; { Entry.last = "Smith"; first = [ "A." ] }
       ; { Entry.last = "Jones"; first = [ "B." ] }
       ])
    (parse_author_list "Doe, J. and Smith, A. and Jones, B.")
    "three authors"
    show_author_list_result;
  expect_eq
    (Error "John Doe")
    (parse_author_list "John Doe")
    "malformed: no comma"
    show_author_list_result;
  expect_eq
    (Error "John Doe")
    (parse_author_list "John Doe and Smith, A.")
    "malformed first author reported"
    show_author_list_result;
  expect_eq
    (Error "John Smith")
    (parse_author_list "Doe, J. and John Smith")
    "malformed second author reported"
    show_author_list_result
;;

let show_common_fields_result r = [%show: (Entry.common_fields, error) result] r

let make_full_entry tag =
  make_entry_with_fields
    tag
    [| field "author" "Doe, J."
     ; field "title" "A Great Paper"
     ; int_field "year" 2024
     ; field "archive" "arxiv"
    |]
;;

let test_get_common_fields () =
  expect_eq
    (Ok
       { Entry.author = [ { Entry.last = "Doe"; first = [ "J." ] } ]
       ; title = "A Great Paper"
       ; year = 2024
       ; archive = "arxiv"
       })
    (get_common_fields (make_full_entry "test"))
    "happy path"
    show_common_fields_result;
  let missing_title =
    make_entry_with_fields
      "test"
      [| field "author" "Doe, J."; int_field "year" 2024; field "archive" "arxiv" |]
  in
  expect_eq
    (Error (MissingField (Entry.Key "title", Entry.Tag "test")))
    (get_common_fields missing_title)
    "missing title"
    show_common_fields_result;
  let missing_author =
    make_entry_with_fields
      "test"
      [| field "title" "A Great Paper"; int_field "year" 2024; field "archive" "arxiv" |]
  in
  expect_eq
    (Error (MissingField (Entry.Key "author", Entry.Tag "test")))
    (get_common_fields missing_author)
    "missing author"
    show_common_fields_result;
  let year_wrong_type =
    make_entry_with_fields
      "test"
      [| field "author" "Doe, J."
       ; field "title" "A Great Paper"
       ; field "year" "twenty twenty four"
       ; field "archive" "arxiv"
      |]
  in
  expect_eq
    (Error
       (ValueTypeMismatch
          ( Entry.Key "year"
          , Entry.Tag "test"
          , Expected Entry.Value.KInteger
          , Actual Entry.Value.KString )))
    (get_common_fields year_wrong_type)
    "year must be integer"
    show_common_fields_result;
  let malformed_author =
    make_entry_with_fields
      "test"
      [| field "author" "John Smith"
       ; field "title" "A Great Paper"
       ; int_field "year" 2024
       ; field "archive" "arxiv"
      |]
  in
  expect_eq
    (Error (MalformedAuthorName ("John Smith", Entry.Tag "test")))
    (get_common_fields malformed_author)
    "author without comma"
    show_common_fields_result
;;

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () =
  run_test "assert_no_duplicate_entry" test_assert_no_duplicate_entry;
  run_test "assert_no_duplicate_field" test_assert_no_duplicate_field;
  run_test "locate_field" test_locate_field;
  run_test "parse_author_list" test_parse_author_list;
  run_test "get_common_fields" test_get_common_fields
;;
