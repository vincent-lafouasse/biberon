type error =
  | MalformedEntry
  | DuplicateField
  | UnknownEtype
  | MissingCoreField
  | MissingSpecificField

let validate_entry (_raw_entry : Entry.raw_entry) : (Entry.bib_entry, error) result =
  failwith "todo"
;;

let validate_entry (_raw_lib : Entry.raw_entry array)
  : (Entry.bib_entry array, error) result
  =
  failwith "todo"
;;
