type t =
  { absolute : int
  ; line : int
  ; column : int
  }
[@@deriving show]

type 'a located = 'a * t

let distance pos_from pos_to = pos_to.absolute - pos_from.absolute
