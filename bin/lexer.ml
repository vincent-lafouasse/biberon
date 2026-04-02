[@@@warning "-69-34-37-32"]

type t =
  { input : string
  ; position : Position.t
  }
[@@deriving show]

type error = UnterminatedString [@@deriving show]

let init (input : string) : t =
  let position : Position.t = { absolute = 0; line = 1; column = 0 } in
  { input; position }
;;

let len lexer = String.length lexer.input

let at lexer (position : Position.t) = String.get lexer.input position.absolute

let eof lexer = lexer.position.absolute >= len lexer

let get (lexer : t) : char option =
  if eof lexer then None else Some (at lexer lexer.position)
;;

let get_unsafe lexer : char = Option.get (get lexer)

let substring lexer (start_pos : Position.t) (end_pos : Position.t) =
  String.sub lexer.input start_pos.absolute (Position.distance start_pos end_pos)
;;
