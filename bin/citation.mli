type t

type style = IEEE

type target = Markdown

val with_style : Entry.t -> style -> t

val format : t -> target -> string
