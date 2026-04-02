type t
type error

val init : string -> t
val next_token : t -> t * (Token.t Position.located, error Position.located) result
val show : t -> string
val __test : unit -> unit
