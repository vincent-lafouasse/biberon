type error

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string

val parse : string -> (Entry.raw_entry Array.t, error Position.located) result

val __test : unit -> unit
