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
