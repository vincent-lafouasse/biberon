open Entry

type 'a expected = Expected of 'a [@@deriving show]
type 'a actual = Actual of 'a [@@deriving show]

type error =
  | DuplicateEntry of tag
  | DuplicateField of key * tag
  | ValueTypeMismatch of key * tag * Value.kind expected * Value.kind actual
  | MissingCoreField of key * tag
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
  | _ -> failwith "unwrapped wrong value type"
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
    (* NOTE: wait i don't know if its a core field yet hmmmm *)
    Option.to_result ~none:(MissingCoreField (key, entry.tag)) maybe_value
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

let get_common_fields (raw_entry : raw_entry) : (common_fields, error) result =
  (* could probably take that into a get_string_field function *)
  let maybe_title = locate_field raw_entry "title" in
  let title_res =
    Option.to_result ~none:(MissingCoreField (Key "title", raw_entry.tag)) maybe_title
  in
  let check_type (expected_kind : Value.kind) (value : Value.t) : (Value.t, error) result =
    if Value.kind_of value = expected_kind
    then Ok value
    else (
      let expected = Expected expected_kind in
      let actual = Actual (Value.kind_of value) in
      Error (ValueTypeMismatch (Key "title", raw_entry.tag, expected, actual)))
  in
  let title_res = Result.bind title_res (check_type Value.KString) in
  let title_res : (string, error) result = Result.map unwrap_string title_res in
  let _ = title_res in
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

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () =
  run_test "assert_no_duplicate_entry" test_assert_no_duplicate_entry;
  run_test "assert_no_duplicate_field" test_assert_no_duplicate_field;
  run_test "locate_field" test_locate_field
;;
