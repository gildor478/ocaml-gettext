open OUnit;;

open FileUtil;;
(* BUG: should be independant of Str *)
open FileUtil.StrUtil;;
open FilePath;;
open FilePath.DefaultPath;;

open GettextTypes;;

type tests = {
  verbose        : bool;
  search_path    : string list;
  ocaml_xgettext : string;
  test_dir       : string;
}
;;

let parse_arg () = 
  let tests = ref { 
    verbose        = false; 
    search_path    = []; 
    ocaml_xgettext = make_filename [ parent_dir ; "build" ; "bin" ; "ocaml-xgettext"];
    test_dir       = make_filename [ current_dir ; "tests" ];
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
    (
      "-test-dir", Arg.String (
        fun s ->
          tests := { !tests with test_dir = s }
        )
      ,"Specify the temporary dir for testing files"
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

let string_of_translation trans = 
  match trans with
    Singular(str_id, str) ->
      Printf.sprintf "Singular(%S, %S)" str_id str
  | Plural(str_id, str_plural, lst) ->
      Print.sprintf "Plural(%S, %S, [ %a ])" str_id str_plural 
      (String.concat " ; " (List.map (fun x -> Printf.sprintf "%S" x) lst)) 
;;

let load_mo_file tests fl_mo = 
  [
    "Loading ( header )" >::
    ( fun () ->
      try
        let mo = open_in_bin fl_mo
        in
        let mo_header = GettextMo.input_mo_header mo
        in
        print_debug tests (GettextMo.string_of_mo_header mo_header);
        close_in mo
      with x ->
        assert_failure (fl_mo^" doesn't load properly: "^(GettextMo.string_of_exception x))
    );

    "Loading ( informations )" >::
    ( fun () ->
      try
        let mo = open_in_bin fl_mo
        in
        let mo_header = GettextMo.input_mo_header mo
        in
        let mo_informations = GettextMo.input_mo_informations
          GettextTypes.RaiseException mo mo_header
        in
        print_debug tests (GettextMo.string_of_mo_informations mo_informations);
        close_in mo
      with x ->
        assert_failure (fl_mo^" doesn't load properly: "^(GettextMo.string_of_exception x))
    );
  ]
;;

let load_po_file tests fl_po = 
  (* BUG : should use add_extension *)
  let fl_mo = concat tests.test_dir ((chop_extension fl_po)^".mo")
  in
  [
    "Parsing" >:: 
      ( fun () -> 
        try 
          let chn = open_in fl_po
          in
          ignore (GettextPo.input_po chn);
          close_in chn
        with x ->
          assert_failure (fl_po^" doesn't parse correctly: "^(GettextPo.string_of_exception x))
      );

      "Compiling" >::
      ( fun () ->
        try
          let _ = GettextCompile.compile fl_po fl_mo
          in
          () 
        with x ->
          assert_failure (fl_po^" doesn't compile correctly"^(GettextCompile.string_of_exception x))
      );
  ] @ (load_mo_file tests fl_mo)
;;

(**********************************)
(* Test of Printf format checking *)
(**********************************)

let format_test tests =
  let format_test_one trans_src trans_dst =
    let str_id = 
      match trans_src with
        Singular(str_id,_)
      | Plural(str_id,_,_) -> str_id
    in
    (Print.sprintf "%S format checking" str_id) >::
      ( fun () ->
        let trans_res = check_format Ignore trans_src
        in
        match (trans_res,trans_src) with
          Singular(str_id1,str1),Singular(str_id2,str2) when 
          str_id1 = str_id2 && str1 = str2 ->
            ()
        | Plural(str_id1,str_plural1,lst1),Plural(str_id2,str_plural2,lst2) when 
          str_id1 = str_id2 && str_plural1 = str_plural2 && List.fold


(*************************)
(* Test of PO processing *)
(*************************)

let po_test tests = 
  let po_test_one fl_po = 
    fl_po >:::
      load_po_file tests fl_po
  in
  "PO processing test" >:::
    List.map po_test_one ["test1.po"; "test2.po" ; "test3.po"; "test4.po"]
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
  let default_options = "-I +camlp4 pa_o.cmo"
  in
  let filename_options = MapString.empty
  in
  let extract_test_one fl_ml = 
    (* BUG : should use add_extension *)
    let fl_pot = concat tests.test_dir ((chop_extension fl_ml)^".pot")
    in
    fl_ml >:::
      [ 
        "Extracting" >::
          ( fun () ->
            try
              GettextCompile.extract tests.ocaml_xgettext default_options filename_options [fl_ml] fl_pot
            with x ->
              assert_failure (fl_ml^" doesn't extract correctly: "^(GettextCompile.string_of_exception x))
          )
      ]
  in
  "Ocaml file extraction test" >:::
    List.map extract_test_one [ "test4.ml" ]
;;

(********************************)
(* Test of MO file installation *)
(********************************)

let install_test tests =
  let install_test_one (language, category, textdomain, fl_mo, fl_dst) =  
    fl_mo >:::
      [
        "Installing" >::
          ( fun () ->
            try 
              GettextCompile.install tests.test_dir language category textdomain fl_mo;
              if test Exists fl_dst then
                ()
              else
                assert_failure (fl_mo^" is not installed at "^fl_dst)
            with x ->
              assert_failure ("Unexpected error while processing "^fl_mo
              ^" ( "^(Printexc.to_string x)^" )")
          )
      ]
  in
  "MO file installation test" >:::
    List.map install_test_one [
      (
        "fr",LC_MESSAGES, "gettext-test1", concat tests.test_dir "test1.mo", 
        make_filename [ tests.test_dir ; "fr" ; "LC_MESSAGES" ; "gettext-test1.mo" ]
      );
      (
        "fr_FR",LC_MESSAGES, "gettext-test2", concat tests.test_dir "test2.mo", 
        make_filename [ tests.test_dir ; "fr_FR" ; "LC_MESSAGES" ; "gettext-test2.mo"]
      );
      (
        "fr",LC_TIME, "gettext-test3", concat tests.test_dir "test3.mo", 
        make_filename [ tests.test_dir ; "fr" ; "LC_TIME" ; "gettext-test3.mo" ]
      );
      (
        "fr_FR@euro",LC_MESSAGES, "gettext-test4", concat tests.test_dir "test4.mo",
        make_filename [ tests.test_dir ; "fr_FR@euro" ; "LC_MESSAGES" ; "gettext-test4.mo" ]
      );
    ]
;;

(************************)
(* Test of POT/PO merge *)
(************************)

let merge_test tests = 
  let merge_one (fl_pot,fl_po,backup_ext) = 
    (fl_pot^"+"^fl_po) >:::
      [
        "Merging" >::
          ( fun () ->
            try
              (* Copying the file to the good place *)
              let fl_po_cp = 
                concat tests.test_dir fl_po
              in
              let () = 
                cp [fl_po] fl_po_cp
              in
              let fl_backup = 
                (* BUG : should use add_extension *)
                fl_po_cp^"."^backup_ext
              in
              GettextCompile.merge fl_pot [fl_po_cp] backup_ext;
              (
                match cmp fl_po fl_po_cp with
                  Some -1 -> 
                    assert_failure (fl_po^" or "^fl_po_cp^" doesn't exist")
                | Some x ->
                    assert_failure (fl_po^" differs from "^fl_po_cp)
                | None ->
                    ()
              );
              (
                match cmp fl_po fl_backup with
                  Some -1 -> 
                    assert_failure (fl_po^" or "^fl_backup^" doesn't exist")
                | Some x ->
                    assert_failure (fl_po^" differs from "^fl_backup)
                | None ->
                    ()
              )
            with x ->
              assert_failure ("Unexpected error while processing "^fl_po
              ^" ( "^(Printexc.to_string x)^" )")
          );
      ]
  in
  "POT/PO file merge test" >:::
    List.map merge_one [ (concat tests.test_dir "test4.pot","test4.po", "bak") ]
;;

(*********************)
(* Main test routine *)
(*********************)

let tests = parse_arg ()
in
let all_test = 
  "Test ocaml-gettext" >::: 
    [
      po_test            tests; 
      compatibility_test tests;
      extract_test       tests;
      install_test       tests;
      merge_test         tests;
    ]
in
let () = 
  print_endline ("Test            : ocaml-gettext "^(GettextConfig.version));
  print_endline ("Test build date : "^(GettextConfig.build_date));
  print_endline ("OS              : "^(Sys.os_type));
  print_endline ("Creating "^(tests.test_dir)^"...");
  mkdir ~parent:true tests.test_dir;
  print_endline ("Running...")
in
run_test_tt all_test

