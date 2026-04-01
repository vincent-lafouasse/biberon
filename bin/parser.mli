type t
type error

val init : string -> t
val next_raw_entry : t -> t * (Entry.raw_entry option, error) result
val show : t -> string
val __test : unit -> unit
