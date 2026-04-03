open Entry

type 'a expected = Expected of 'a [@@deriving show]
type 'a actual = Actual of 'a [@@deriving show]

type error =
  | DuplicateEntry of tag
  | DuplicateField of key * tag
  | ValueTypeMismatch of key * tag * Value.kind expected * Value.kind actual
  | MissingField of key * tag
  | MalformedAuthorName of string * tag
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

let assert_no_duplicate_entry (raw_lib : raw_entry array) : (unit, error) result =
  let tag_of (entry : raw_entry) : string =
    let (Tag tag) = entry.tag in
    tag
  in
  let maybe_duplicate : string option = find_duplicates tag_of raw_lib in
  match maybe_duplicate with
  | None -> Ok ()
  | Some tag -> Error (DuplicateEntry (Tag tag))
;;

let assert_no_duplicate_field (raw_entry : raw_entry) : (unit, error) result =
  let field_name (field : field) : string =
    let key, _value = field in
    let (Key key_name) = key in
    key_name
  in
  let maybe_duplicate : string option = find_duplicates field_name raw_entry.fields in
  match maybe_duplicate with
  | None -> Ok ()
  | Some key_name -> Error (DuplicateField (Key key_name, raw_entry.tag))
;;

let locate_field (raw_entry : raw_entry) (field_name : string) : Value.t option =
  let maybe_field : field option =
    Array.find_opt (fun ((key, _value) : field) -> key = Key field_name) raw_entry.fields
  in
  Option.map (fun (_key, value) -> value) maybe_field
;;

(* probably unsafe, just be careful. should only be called after type check *)
let unwrap_string (value : Value.t) : string =
  match value with
  | Value.String str -> str
  | _ -> failwith "unwrap_string on non string value"
;;

let unwrap_int (value : Value.t) : int =
  match value with
  | Value.Integer i -> i
  | _ -> failwith "unwrap_int on non int value"
;;

let get_string_field (entry : raw_entry) (key : key) : (string, error) result =
  let (Key key_name) = key in
  (* locate *)
  let maybe_value : Value.t option = locate_field entry key_name in
  (* transmute option to error for monadic chaining *)
  let value_res : (Value.t, error) result =
    Option.to_result ~none:(MissingField (key, entry.tag)) maybe_value
  in
  (* err if value is not a string, unwrap it otherwise *)
  let unwrap_string_or_err (value : Value.t) : (string, error) result =
    if Value.kind_of value = Value.KString
    then Ok (unwrap_string value)
    else (
      let expected = Expected Value.KString in
      let actual = Actual (Value.kind_of value) in
      Error (ValueTypeMismatch (key, entry.tag, expected, actual)))
  in
  Result.bind value_res unwrap_string_or_err
;;

let get_int_field (entry : raw_entry) (key : key) : (int, error) result =
  let (Key key_name) = key in
  let maybe_value : Value.t option = locate_field entry key_name in
  let value_res : (Value.t, error) result =
    Option.to_result ~none:(MissingField (key, entry.tag)) maybe_value
  in
  let unwrap_int_or_err (value : Value.t) : (int, error) result =
    if Value.kind_of value = Value.KInteger
    then Ok (unwrap_int value)
    else (
      let expected = Expected Value.KInteger in
      let actual = Actual (Value.kind_of value) in
      Error (ValueTypeMismatch (key, entry.tag, expected, actual)))
  in
  Result.bind value_res unwrap_int_or_err
;;

(* error payload is its own raw input, this way i can keep track of which
   author is malformed. the error will be wrapped in a Validate.error at the
   call site*)
(* returning `MalformedAuthorName of string * tag` means we have to be in a
   scope where we know the entry tag*)
let parse_single_author author_str : (author, string) result =
  let parts : string list = Str.split (Str.regexp ",") author_str in
  let parts : (string * string, string) result =
    match parts with
    | [ last; first ] -> Ok (last, first)
    | _ -> Error author_str
  in
  let split_first_names (parts : string * string) : author =
    let last, first = parts in
    let first : string list = Str.split (Str.regexp " ") first in
    { last; first }
  in
  Result.map split_first_names parts
;;

let parse_author_list (author_list_str : string) : (author list, string) result =
  let raw_author_list = Str.split (Str.regexp "and") author_list_str in
  let author_res_list = List.map parse_single_author raw_author_list in
  match List.find_opt Result.is_error author_res_list with
  | Some bad_name_error ->
    let bad_name = Result.get_error bad_name_error in
    Error bad_name
  | None -> Ok (List.map Result.get_ok author_res_list)
;;

let get_common_fields (entry : raw_entry) : (common_fields, error) result =
  let author_key = Key "author" in
  let title_key = Key "title" in
  let year_key = Key "year" in
  let archive_key = Key "archive" in
  (* try and gather all of them *)
  let author_str_res = get_string_field entry author_key in
  let title_res = get_string_field entry title_key in
  let archive_res = get_string_field entry archive_key in
  let year_res = get_int_field entry year_key in
  (* check if any err'd. if so return any of them. all fields are mandatory so
     any missing field is fatal. not very ergnonomic for users but should be
     fine for now*)
  let _maybe_err : error option =
    match author_str_res with
    | Error e -> Some e
    | _ ->
      (match title_res with
       | Error e -> Some e
       | _ ->
         (match archive_res with
          | Error e -> Some e
          | _ ->
            (match year_res with
             | Error e -> Some e
             | _ -> None)))
  in
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

let make_entry tag = { etype = Etype "misc"; tag = Tag tag; fields = [||] }

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
    (Error (DuplicateEntry (Tag "a")))
    (assert_no_duplicate_entry [| make_entry "a"; make_entry "a" |])
    "single duplication: reports duplicate tag"
    show_result;
  (* duplicate is not the only entry *)
  expect_eq
    (Error (DuplicateEntry (Tag "b")))
    (assert_no_duplicate_entry [| make_entry "a"; make_entry "b"; make_entry "b" |])
    "duplicate among others: reports duplicate tag"
    show_result;
  (* duplicate appears first *)
  expect_eq
    (Error (DuplicateEntry (Tag "a")))
    (assert_no_duplicate_entry [| make_entry "a"; make_entry "b"; make_entry "a" |])
    "duplicate appears first: reports correct tag"
    show_result
;;

let make_entry_with_fields tag fields = { etype = Etype "misc"; tag = Tag tag; fields }

let field k v = Key k, Value.String v

let show_field_result r = [%show: (unit, error) result] r
let show_value_opt r = [%show: Value.t option] r

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
    (Error (DuplicateField (Key "a", Tag "e")))
    (assert_no_duplicate_field dup)
    "duplicate field: reports key and tag"
    show_field_result;
  let dup_not_first =
    make_entry_with_fields "e" [| field "a" "1"; field "b" "2"; field "b" "3" |]
  in
  expect_eq
    (Error (DuplicateField (Key "b", Tag "e")))
    (assert_no_duplicate_field dup_not_first)
    "duplicate not first: reports correct key"
    show_field_result
;;

let test_locate_field () =
  let entry =
    make_entry_with_fields "e" [| field "author" "Doe"; field "year" "2024" |]
  in
  expect_eq
    (Some (Value.String "Doe"))
    (locate_field entry "author")
    "found: author"
    show_value_opt;
  expect_eq
    (Some (Value.String "2024"))
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

let show_author_list_result r = [%show: (author list, string) result] r

let test_parse_author_list () =
  (* single author, comma format *)
  expect_eq
    (Ok [ { last = "Doe"; first = [ "J." ] } ])
    (parse_author_list "Doe, J.")
    "single author comma format"
    show_author_list_result;
  (* single author, multiple first names *)
  expect_eq
    (Ok [ { last = "Doe"; first = [ "John"; "A." ] } ])
    (parse_author_list "Doe, John A.")
    "single author multiple first names"
    show_author_list_result;
  (* two authors *)
  expect_eq
    (Ok [ { last = "Doe"; first = [ "J." ] }; { last = "Smith"; first = [ "A." ] } ])
    (parse_author_list "Doe, J. and Smith, A.")
    "two authors"
    show_author_list_result;
  (* three authors *)
  expect_eq
    (Ok
       [ { last = "Doe"; first = [ "J." ] }
       ; { last = "Smith"; first = [ "A." ] }
       ; { last = "Jones"; first = [ "B." ] }
       ])
    (parse_author_list "Doe, J. and Smith, A. and Jones, B.")
    "three authors"
    show_author_list_result;
  (* malformed: no comma, single author *)
  expect_eq
    (Error "John Doe")
    (parse_author_list "John Doe")
    "malformed: no comma"
    show_author_list_result;
  (* malformed: first author is bad, second is good — finds first *)
  expect_eq
    (Error "John Doe")
    (parse_author_list "John Doe and Smith, A.")
    "malformed first author reported"
    show_author_list_result;
  (* malformed: first is good, second is bad — finds first bad *)
  expect_eq
    (Error "John Smith")
    (parse_author_list "Doe, J. and John Smith")
    "malformed second author reported"
    show_author_list_result
;;

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () =
  run_test "assert_no_duplicate_entry" test_assert_no_duplicate_entry;
  run_test "assert_no_duplicate_field" test_assert_no_duplicate_field;
  run_test "locate_field" test_locate_field;
  run_test "parse_author_list" test_parse_author_list
;;
