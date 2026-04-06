[@@@warning "-69-34-37-32"]

let die msg =
  print_endline msg;
  exit 1
;;

let parse_or_die input =
  let parse_res = Parser.parse input in
  match parse_res with
  | Ok entries -> entries
  | Error err -> die (Parser.format_located_error err input)
;;

let validate_or_die lib =
  let lib_res = Validate.validate_library lib in
  match lib_res with
  | Ok lib -> lib
  | Error err -> die (Validate.format_full_error err)
;;

let () =
  let path =
    match Array.length Sys.argv with
    | 2 -> Sys.argv.(1)
    | _ -> die "Usage: biberon refs.bib"
  in
  let read_file (path : string) : string =
    In_channel.with_open_bin path In_channel.input_all
  in
  let input = read_file path in
  let lib = parse_or_die input in
  let lib = validate_or_die lib in
  let print_entry (_tag, entry) =
    let citation = Citation.with_style entry Citation.IEEE in
    let formatted_citation = Citation.format citation Citation.Markdown in
    print_endline formatted_citation
  in
  List.iter print_entry lib
;;
