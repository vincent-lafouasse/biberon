type parser

val init : string -> parser option

val next_raw_entry : parser -> parser * Entry.raw_entry option
