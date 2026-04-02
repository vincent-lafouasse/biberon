type t =
  { absolute : int
  ; line : int
  ; column : int
  }
[@@deriving show]

let distance pos_from pos_to = pos_to.absolute - pos_from.absolute
