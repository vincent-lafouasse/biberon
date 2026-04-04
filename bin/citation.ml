type modifier =
  | Normal
  | Bold
  | Italic
  | SmallCaps

type blob =
  | Text of
      { text : string
      ; modifier : modifier
      }
  | Link of
      { display : string
      ; url : string
      }

type t = blob list

type segment =
  | Plain of string
  | Verbatim of string

let parse_segments (s : string) : segment list =
  let n = String.length s in
  let rec go i plain acc =
    if i >= n
    then List.rev (if plain = "" then acc else Plain plain :: acc)
    else (
      match s.[i] with
      | '\\' ->
        let escaped = if i + 1 < n then String.make 1 s.[i + 1] else "" in
        go (i + 2) (plain ^ escaped) acc
      | '{' ->
        let acc = if plain = "" then acc else Plain plain :: acc in
        let rec read_verbatim j verbatim =
          if j >= n
          then verbatim, j
          else (
            match s.[j] with
            | '}' -> verbatim, j + 1
            | c -> read_verbatim (j + 1) (verbatim ^ String.make 1 c))
        in
        let verbatim, j = read_verbatim (i + 1) "" in
        go j "" (Verbatim verbatim :: acc)
      | c -> go (i + 1) (plain ^ String.make 1 c) acc)
  in
  go 0 "" []
;;

let ieee_format_author (author : Entry.author) : string =
  let initials = List.map (fun name -> String.sub name 0 1 ^ ".") author.first in
  String.concat " " initials ^ " " ^ author.last
;;

let ieee_format_author_list (author_list : Entry.author list) : blob =
  let text =
    match author_list with
    | [] -> ""
    | _ when List.length author_list >= 7 ->
      ieee_format_author (List.hd author_list) ^ " et al."
    | _ ->
      let formatted = List.map ieee_format_author author_list in
      (match formatted with
       | [] -> ""
       | [ single ] -> single
       | _ ->
         let all_but_last =
           List.filteri (fun i _ -> i < List.length formatted - 1) formatted
         in
         let last = List.nth formatted (List.length formatted - 1) in
         String.concat ", " all_but_last ^ ", and " ^ last)
  in
  Text { text; modifier = Normal }
;;
