[@@@warning "-69-34-37-32"]

let die msg =
  print_endline msg;
  exit 1
;;

let lamport1983 =
  {|
@article{Lamport1983,
	author     = "Lamport, Leslie",
	title      = "Specifying Concurrent Program Modules",
	year       = 1983,

	journal    = "ACM Trans. Program. Lang. Syst.",
	volume     = 5,
	number     = 2,
	pages      = "190--222",
	month      = "apr",

	issue_date = "April 1983",
	publisher  = "Association for Computing Machinery",
	address    = "New York, NY, USA",
	issn       = "0164-0925",
	numpages   = "33",

	doi        = "10.1145/69624.357207",
	archive    = "https://lamport.azurewebsites.net/pubs/spec.pdf",
	note       = "foundational proof of correctness of SPSC queues",
}
|}
;;

let () =
  let () = Printf.printf "%s\n" lamport1983 in
  let lib = Parser.parse lamport1983 in
  let lib = Result.get_ok lib in
  let raw_entry = Array.get lib 0 in
  let entry_res = Validate.validate_entry raw_entry in
  let _tag, entry = Result.get_ok entry_res in
  let citation = Citation.with_style entry Citation.IEEE in
  let formatted_citation = Citation.format citation Citation.Markdown in
  print_endline formatted_citation
;;
