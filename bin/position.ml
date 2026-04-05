type t =
  { absolute : int
  ; line : int
  ; column : int
  }
[@@deriving show]

type 'a located = 'a * t [@@deriving show]

let distance pos_from pos_to = pos_to.absolute - pos_from.absolute

let report_error (location : t) (input : string) (message : string) : string =
  let lines : string list = Str.split (Str.regexp "\n") input in
  let faulty_line = List.nth lines (location.line - 1) in
  let line_marker = string_of_int location.line ^ " | " in
  let marked_faulty_line = line_marker ^ faulty_line in
  let error_pointer_col : int = String.length line_marker + location.column in
  let error_pointer = String.make error_pointer_col '~' ^ "^" in
  marked_faulty_line ^ "\n" ^ error_pointer ^ "\n" ^ message
;;
