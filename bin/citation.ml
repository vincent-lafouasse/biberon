type modifier =
  | Normal
  | Bold
  | Italic
  | SmallCaps

type blob =
  { text : string
  ; modifier : modifier
  }

type t = blob list
