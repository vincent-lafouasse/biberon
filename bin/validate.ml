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
