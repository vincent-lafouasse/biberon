[@@@warning "-69-34-37-32"]

let die msg =
  print_endline msg;
  exit 1
;;

let tiny_bib =
  {|
@misc {an_entry,
	a = "A",
	b = 67,
	c = true,
}
|}
;;

let print_tokens tokens =
  Array.iter
    (fun (tok, pos) -> Printf.printf "%s @ %s\n" (Token.show tok) (Position.show pos))
    tokens
;;

let print_entries =
  Array.iter (fun entry -> Printf.printf "%s\n" (Entry.show_raw_entry entry))
;;

let () =
  let () = Printf.printf "%s\n" tiny_bib in
  let maybe_entries = Parser.parse tiny_bib in
  match maybe_entries with
  | Error (err, loc) ->
    die (Printf.sprintf "error at %s: %s" (Position.show loc) (Parser.show_error err))
  | Ok entries -> print_entries entries
;;
