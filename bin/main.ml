[@@@warning "-69-34-37-32"]

let die msg =
  print_endline msg;
  exit 1
;;

let lexer_from_file_or_die (path : string) : Lexer.t =
  (* at some point i'll probably wrap this so it errs instead of throwing
   but for now i'll just assume everything went ok, i'll learn to deal with
   exceptions later *)
  let read_file (path : string) : string =
    In_channel.with_open_bin path In_channel.input_all
  in
  let input = read_file path in
  Lexer.init input
;;

let () = Lexer.__test ()
