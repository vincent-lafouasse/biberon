type error =
  | DuplicateEntry of string
  | MalformedEntry
  | DuplicateField
  | UnknownEtype
  | MissingCoreField
  | MissingSpecificField

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

let assert_no_duplicates (raw_lib : Entry.raw_entry array) : (unit, error) result =
  let update_count map entry =
    let count =
      match StringMap.find_opt entry.tag with
      | None -> 1
      | Some count -> count + 1
    in
    StringMap.add entry.tag count map
  in
  let frequency_map = Array.fold_left update_count raw_lib StringMap.empty in
  let is_duplicate : string -> int -> bool = fun (_tag, count) -> count > 1 in
  let duplicates = StringMap.filter is_duplicate frequency_map in
  match StringMap.find_first_opt (fun _ -> true) duplicates with
  | None -> Ok ()
  | Some tag -> Error (DuplicateEntry tag)
;;

let validate_entry (_raw_entry : Entry.raw_entry) : (Entry.bib_entry, error) result =
  failwith "todo"
;;

let validate_entry (_raw_lib : Entry.raw_entry array) : (Entry.t array, error) result =
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

let make_entry tag =
  { Entry.etype = Entry.Etype "misc"; tag = Entry.Tag tag; fields = [||] }
;;

let test_assert_no_duplicates () =
  (* empty library *)
  expect_eq (Ok ()) (assert_no_duplicates [||]) "empty: no duplicates" show_result;
  (* single entry *)
  expect_eq
    (Ok ())
    (assert_no_duplicates [| make_entry "a" |])
    "single entry: no duplicates"
    show_result;
  (* multiple distinct entries *)
  expect_eq
    (Ok ())
    (assert_no_duplicates [| make_entry "a"; make_entry "b"; make_entry "c" |])
    "distinct entries: no duplicates"
    show_result;
  (* single duplication *)
  expect_eq
    (Error (DuplicateEntry "a"))
    (assert_no_duplicates [| make_entry "a"; make_entry "a" |])
    "single duplication: reports duplicate tag"
    show_result;
  (* duplicate is not the only entry *)
  expect_eq
    (Error (DuplicateEntry "b"))
    (assert_no_duplicates [| make_entry "a"; make_entry "b"; make_entry "b" |])
    "duplicate among others: reports duplicate tag"
    show_result;
  (* duplicate appears first *)
  expect_eq
    (Error (DuplicateEntry "a"))
    (assert_no_duplicates [| make_entry "a"; make_entry "b"; make_entry "a" |])
    "duplicate appears first: reports correct tag"
    show_result
;;

let run_test name f =
  f ();
  print_endline (name ^ " ok")
;;

let __test () = run_test "assert_no_duplicates" test_assert_no_duplicates
