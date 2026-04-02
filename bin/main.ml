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
	c = true
}
|}
;;

let tokens = Lexer.tokenize tiny_bib
let tokens = Result.get_ok tokens

let print_tokens tokens =
  Array.iter (fun (tok, _pos) -> Printf.printf "%s\n" (Token.show tok)) tokens
;;

let () = print_tokens tokens

let () = Lexer.__test ()
let () = Parser.__test ()
