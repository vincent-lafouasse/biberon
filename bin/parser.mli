type error

val parse : string -> (Entry.raw_entry Array.t, error Position.located) result

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string

val __test : unit -> unit
