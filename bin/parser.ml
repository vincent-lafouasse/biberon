type parser = { input : string; position : int; ch : char option }

let init (input : string) : parser option = failwith "todo"
let eof (parser : parser) : bool = failwith "todo"
let advance (parser : parser) : parser = failwith "todo"

(* parser -> parser * string *)
let expect_identifier (parser : parser) = failwith "todo"

(* parser -> char -> parser option *)
let expect_char (parser : parser) (_c : char) = failwith "todo"

(* parser -> parser * Entry.raw_entry option *)
let next_raw_entry (parser : parser) = failwith "todo"
