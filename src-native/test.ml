open FileUtil;;
open FileUtil.StrUtil;;
open Camlgettext;;

let read_one_mo file = 
  try 
    let _ = print_endline file 
    in
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
Arg.parse 
[
  ("-search", (Arg.String( fun s -> 
      print_endline ("Searching "^s);
      find (Has_extension "mo") s (fun () fln -> read_one_mo fln) ()
      )
    ), "Search the directory for .mo file")
] 
(fun str -> read_one_mo str )
"Camlgettext v0.2 Sylvain Le Gall"
;;
