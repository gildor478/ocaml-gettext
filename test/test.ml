open OUnit;;

open FileUtil;;
open FileUtil.StrUtil;;

open GettextMo;;
open GettextPo;;

(*
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
  with GettextTypes.Bad_mo_file ->
    print_endline "Bad mo file"
in
Arg.parse 
[
  ("-search", (Arg.String( fun s -> 
      print_string ("Searching "^s);
      print_newline ();
      find (Has_extension "mo") s (fun () fln -> read_one_mo fln) ()
      )
    ), "Search the directory for .mo file")
] 
(fun str -> read_one_mo str )
"Camlgettext v0.2 Sylvain Le Gall"
;;
*)

let po_test = 
  let po_test_one fl = 
    fl >:::
      [
        "Parsing" >:: 
        ( fun () -> 
          try 
            let _ = po_of_file fl
            in
            ()
          with x ->
            print_endline (GettextPo.string_of_exception x);
            assert_failure (fl^" doesn't parse correctly")
        );

        "Compiling" >::
        ( fun () ->
          try
            let _ = compile_po (po_of_file fl)
            in
            () 
          with x ->
            print_endline (GettextPo.string_of_exception x);
            assert_failure (fl^" doesn't compile correctly")
        )
      ]
  in
  "Self test" >:::
    List.map po_test_one ["test1.po"; "test2.po" ; (*"test3.po"*)]
in
let all_test = po_test
in
let _ = 
  print_endline ("Test            : gettext "^(Version.version));
  print_endline ("Test build date : "^(Version.date));
  print_endline ("OS              : "^(Sys.os_type));
  print_endline ("Running...")
in
run_test_tt_main all_test


