type t
type error

val pp : Format.formatter -> t -> unit
val show : t -> string
val pp_error : Format.formatter -> error -> unit
val show_error : error -> string

val tokenize : string -> (Token.t Position.located array, error Position.located) result
val __test : unit -> unit
