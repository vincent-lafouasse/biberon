type t

val init : string -> t option

val next_raw_entry : t -> t * Entry.raw_entry option
