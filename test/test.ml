open OUnit;;

open FileUtil;;
open FileUtil.StrUtil;;
open FilePath;;
open FilePath.DefaultPath;;

open GettextMo;;
open GettextPo;;
open GettextCompile;;
open GettextTypes;;

type tests = {
  verbose        : bool;
  search_path    : string list;
  ocaml_xgettext : string;
}
;;

let parse_arg () = 
  let tests = ref { 
    verbose = false; 
    search_path = []; 
    ocaml_xgettext = make_filename [ parent_dir ; "build" ; "bin" ; "ocaml-xgettext"]
  }
  in
  Arg.parse [
    (
      "-search", Arg.String ( 
        fun dir -> 
          tests := { !tests with search_path = dir :: !tests.search_path }
        )
      ,"Search the specified directory for .mo file"
    );
    (
      "-verbose", Arg.Unit (
        fun () ->
          tests := { !tests with verbose = true }
        )
      ,"Processs with a lot of message"
    );
    (
      "-ocaml-xgettext", Arg.String (
        fun s ->
          tests := { !tests with ocaml_xgettext = s }
        )
      ,"Specify the ocaml-xgettext executable"
    );
  ]
  (fun str -> () )
  ("Test utility for ocaml-gettext v"^(GettextConfig.version)^" by Sylvain Le Gall\n"^
  "Copyright 2004,2005. Licensed under LGPL v2.1 with Ocaml exception");
  !tests
;;

let print_debug tests str =
  if tests.verbose then
    (print_string str; print_newline ())
  else
    ()
;;

let load_mo_file tests fl_mo = 
  [
    "Loading ( header )" >::
    ( fun () ->
      try
        let mo = open_in_bin fl_mo
        in
        let mo_header = input_mo_header mo
        in
        print_debug tests (string_of_mo_header mo_header);
        close_in mo
      with x ->
        assert_failure (fl_mo^" doesn't load properly: "^(GettextMo.string_of_exception x))
    );

    "Loading ( informations )" >::
    ( fun () ->
      try
        let mo = open_in_bin fl_mo
        in
        let mo_header = input_mo_header mo
        in
        let mo_informations = input_mo_informations
          GettextTypes.RaiseException mo mo_header
        in
        print_debug tests (string_of_mo_informations mo_informations);
        close_in mo
      with x ->
        assert_failure (fl_mo^" doesn't load properly: "^(GettextMo.string_of_exception x))
    );
  ]
;;

let load_po_file tests fl_po = 
  (* BUG : should use add_extension *)
  let fl_mo = ((chop_extension fl_po)^".mo")
  in
  [
    "Parsing" >:: 
      ( fun () -> 
        try 
          let chn = open_in fl_po
          in
          ignore (input_po chn);
          close_in chn
        with x ->
          assert_failure (fl_po^" doesn't parse correctly: "^(GettextPo.string_of_exception x))
      );

      "Compiling" >::
      ( fun () ->
        try
          let _ = compile fl_po fl_mo
          in
          () 
        with x ->
          assert_failure (fl_po^" doesn't compile correctly"^(GettextCompile.string_of_exception x))
      );
  ] @ (load_mo_file tests fl_mo)
;;


(*************************)
(* Test of PO processing *)
(*************************)

let po_test tests = 
  let po_test_one fl_po = 
    fl_po >:::
      load_po_file tests fl_po
  in
  "PO processing test" >:::
    List.map po_test_one ["test1.po"; "test2.po" ; (*"test3.po"*)]
;;


(****************************************************)
(* Test compatibility with already produced mo file *)
(****************************************************)

let compatibility_test tests =
  let test_one_mo fl =
    fl >::: (load_mo_file tests fl)
  in
  "Test compatibility" >:::
    List.fold_left ( 
      fun lst dir -> 
        find (Has_extension "mo") dir (fun lst fln -> (test_one_mo fln) :: lst ) lst
    ) [] tests.search_path
;;

(*******************************************)
(* Test of Ocaml source file PO extraction *)
(*******************************************)

let extract_test tests = 
  let default_options = ""
  in
  let filename_options = MapString.empty
  in
  let extract_test_one fl_ml = 
    (* BUG : should use add_extension *)
    let fl_pot = ((chop_extension fl_ml)^".pot")
    in
    fl_ml >:::
      [ 
        "Extracting" >::
          ( fun () ->
            try
              extract tests.ocaml_xgettext default_options filename_options [fl_ml] fl_pot
            with x ->
              assert_failure (fl_ml^" doesn't extract correctly: "^(GettextCompile.string_of_exception x))
          )
      ]
  in
  "Ocaml file extraction test" >:::
    List.map extract_test_one [ "test4.ml" ]
;;

        
(*********************)
(* Main test routine *)
(*********************)

let tests = parse_arg ()
in
let all_test = 
  "Test ocaml-gettext" >::: 
    [
      po_test tests; 
      compatibility_test tests;
      extract_test tests;
    ]
in
let _ = 
  print_endline ("Test            : ocaml-gettext "^(GettextConfig.version));
  print_endline ("Test build date : "^(GettextConfig.build_date));
  print_endline ("OS              : "^(Sys.os_type));
  print_endline ("Running...")
in
run_test_tt all_test

