type error =
  | DuplicateEntry of Entry.tag
  | MalformedEntry
  | DuplicateField of Entry.key * Entry.tag
  | UnknownEtype
  | MissingCoreField
  | MissingSpecificField
[@@deriving show]

module StringMap = Map.Make (String)

(* responsibilities:
    - recognize the etype
    - assert existence and unicity of common and specific fields
    - assert that those fields are not malformed
    - package them in a finished entry
*)

(* before all that, i might check that no two entries have the same tag, this
   way i can use just the tag to report errors
*)

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

let make_entry tag =
  { Entry.etype = Entry.Etype "misc"; tag = Entry.Tag tag; fields = [||] }
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

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () = run_test "assert_no_duplicate_entry" test_assert_no_duplicate_entry
