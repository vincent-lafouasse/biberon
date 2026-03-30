type t = { input : string; position : int; ch : char option }

let init (input : string) : t option = failwith "todo"
let eof (parser : t) : bool = failwith "todo"
let advance (parser : t) : t = failwith "todo"

(* parser -> parser * string *)
let expect_identifier (parser : t) = failwith "todo"

(* parser -> char -> parser option *)
let expect_char (parser : t) (_c : char) = failwith "todo"

(* parser -> parser * Entry.raw_entry option *)
let next_raw_entry (parser : t) = failwith "todo"
