open OUnit;;

open FileUtil;;
open FileUtil.StrUtil;;
open FilePath;;
open FilePath.DefaultPath;;

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

let verbose = ref false 
;;

let po_test = 
  let po_test_one fl = 
    (* BUG : should use extension *)
    let fl_mo = ((chop_extension fl)^".mo")
    in
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
            let _ = compile_po 
              ~default_domain:fl_mo
              (po_of_file fl)
            in
            () 
          with x ->
            print_endline (GettextPo.string_of_exception x);
            assert_failure (fl^" doesn't compile correctly")
        );

        "Loading ( header )" >::
        ( fun () ->
          try
            let mo = open_in_bin fl_mo
            in
            let mo_header = input_mo_header mo
            in
            (
              if !verbose then
                print_endline (string_of_mo_header mo_header)
              else
                ()
            );
            close_in mo
          with x ->
            print_endline (GettextMo.string_of_exception x);
            assert_failure (fl_mo^" doesn't load properly");
        );

        "Loading ( informations ) " >::
        ( fun () ->
          try
            let mo = open_in_bin fl_mo
            in
            let mo_header = input_mo_header mo
            in
            let mo_informations = input_mo_informations
              GettextTypes.RaiseException mo mo_header
            in
            (
              if !verbose then
                print_endline (string_of_mo_informations mo_informations)
              else
                ()
            );
            close_in mo
          with x ->
            print_endline (GettextMo.string_of_exception x);
            assert_failure (fl_mo^" doesn't load properly");
        );
      ]
  in
  "Self test" >:::
    List.map po_test_one ["test1.po"; "test2.po" ; (*"test3.po"*)]
in
let all_test = po_test
in
let _ = 
  print_endline ("Test            : ocaml-gettext "^(GettextConfig.version));
  print_endline ("Test build date : "^(GettextConfig.build_date));
  print_endline ("OS              : "^(Sys.os_type));
  print_endline ("Running...")
in
run_test_tt_main all_test


