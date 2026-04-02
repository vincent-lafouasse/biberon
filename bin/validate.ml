type error =
  | MalformedEntry
  | DuplicateField
  | UnknownEtype
  | MissingCoreField
  | MissingSpecificField

(* responsibilities:
    - recognize the etype
    - assert existence and unicity of common and specific fields
    - assert that those fields are not malformed
    - package them in a finished entry
*)

let validate_entry (_raw_entry : Entry.raw_entry) : (Entry.bib_entry, error) result =
  failwith "todo"
;;

let validate_entry (_raw_lib : Entry.raw_entry array) : (Entry.t array, error) result =
  failwith "todo"
;;
