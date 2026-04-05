type t
type error

val tokenize : string -> (Token.t Position.located array, error Position.located) result

val pp : Format.formatter -> t -> unit
val show : t -> string
val pp_error : Format.formatter -> error -> unit
val show_error : error -> string
val format_error : error -> string
val format_located_error : error Position.located -> string -> string

val __test : unit -> unit
