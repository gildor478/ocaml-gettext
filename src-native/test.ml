
open FileUtil.StrUtil;;
open Camlgettext;;

let file = ref []
in
let _ = 
  Arg.parse 
  [
    ("-search", (Arg.String( fun s -> 
        print_endline ("Searching "^s);
        file := (find (Match(".*\\.mo")) s) @ !file )
      ), "Search the directory for .mo file")
  ] 
  (fun str -> file := str :: !file )
  "Camlgettext v0.2 Sylvain Le Gall"
in
let read_one_mo file = 
  try 
    let mo_file = open_in_bin file
    in
    let header = input_mo_header mo_file
    in
    let translation = input_mo_translation mo_file header
    in
    close_in mo_file 
  with Camlgettext_types.Bad_mo_file ->
    print_endline "Bad mo file"
in
List.iter (fun s -> print_endline s; read_one_mo s) !file
;;
