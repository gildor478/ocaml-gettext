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
      Printf.sprintf "Plural(%S, %S, [ %s ])" str_id str_plural 
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
          RaiseException mo mo_header
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
  let format_test_one (trans_src,trans_dst) =
    let lst_str_equal lst1 lst2 =
      try
        List.fold_left2 ( fun b str1 str2 -> b && str1 = str2 ) 
        true lst1 lst2
      with Invalid_argument _ ->
        false
    in
    let str_id = 
      match trans_src with
        Singular(str_id,_)
      | Plural(str_id,_,_) -> str_id
    in
    (Printf.sprintf "%s -> %s format checking" 
    (string_of_translation trans_src)
    (string_of_translation trans_dst)) >::
      ( fun () ->
        let trans_res = 
          GettextFormat.check_format Ignore trans_src
        in
        match (trans_res,trans_dst) with
          Singular(str_id1,str1),Singular(str_id2,str2) when 
          str_id1 = str_id2 && str1 = str2 ->
            ()
        | Plural(str_id1,str_plural1,lst1),Plural(str_id2,str_plural2,lst2) when 
          str_id1 = str_id2 && str_plural1 = str_plural2 
          && lst_str_equal lst1 lst2 ->
            ()
        | trans1, trans2 ->
            assert_failure ((string_of_translation trans1)
            ^" differs from "
            ^(string_of_translation trans2))
      )
  in
  "Printf format test" >:::
    (
      List.map format_test_one
      [
        (* Identity *)
        Singular("%d" ,"%d" ),Singular("%d" ,"%d" );
        Singular("%i" ,"%i" ),Singular("%i" ,"%i" );
        Singular("%n" ,"%n" ),Singular("%n" ,"%n" );
        Singular("%N" ,"%N" ),Singular("%N" ,"%N" );
        Singular("%u" ,"%u" ),Singular("%u" ,"%u" );
        Singular("%x" ,"%x" ),Singular("%x" ,"%x" );
        Singular("%X" ,"%X" ),Singular("%X" ,"%X" );
        Singular("%o" ,"%o" ),Singular("%o" ,"%o" );
        Singular("%s" ,"%s" ),Singular("%s" ,"%s" );
        Singular("%S" ,"%S" ),Singular("%S" ,"%S" );
        Singular("%c" ,"%c" ),Singular("%c" ,"%c" );
        Singular("%C" ,"%C" ),Singular("%C" ,"%C" );
        Singular("%f" ,"%f" ),Singular("%f" ,"%f" );
        Singular("%F" ,"%F" ),Singular("%F" ,"%F" );
        Singular("%e" ,"%e" ),Singular("%e" ,"%e" );
        Singular("%E" ,"%E" ),Singular("%E" ,"%E" );
        Singular("%g" ,"%g" ),Singular("%g" ,"%g" );
        Singular("%G" ,"%G" ),Singular("%G" ,"%G" );
        Singular("%B" ,"%B" ),Singular("%B" ,"%B" );
        Singular("%b" ,"%b" ),Singular("%b" ,"%b" );
        Singular("%ld","%ld"),Singular("%ld","%ld");
        Singular("%li","%li"),Singular("%li","%li");
        Singular("%lu","%lu"),Singular("%lu","%lu");
        Singular("%lx","%lx"),Singular("%lx","%lx");
        Singular("%lX","%lX"),Singular("%lX","%lX");
        Singular("%lo","%lo"),Singular("%lo","%lo");
        Singular("%nd","%nd"),Singular("%nd","%nd");
        Singular("%ni","%ni"),Singular("%ni","%ni");
        Singular("%nu","%nu"),Singular("%nu","%nu");
        Singular("%nx","%nx"),Singular("%nx","%nx");
        Singular("%nX","%nX"),Singular("%nX","%nX");
        Singular("%no","%no"),Singular("%no","%no");
        Singular("%Ld","%Ld"),Singular("%Ld","%Ld");
        Singular("%Li","%Li"),Singular("%Li","%Li");
        Singular("%Lu","%Lu"),Singular("%Lu","%Lu");
        Singular("%Lx","%Lx"),Singular("%Lx","%Lx");
        Singular("%LX","%LX"),Singular("%LX","%LX");
        Singular("%Lo","%Lo"),Singular("%Lo","%Lo");
        Singular("%a" ,"%a" ),Singular("%a" ,"%a" );
        Singular("%t" ,"%t" ),Singular("%t" ,"%t" );
        Singular("%!" ,"%!" ),Singular("%!" ,"%!" );
        Singular("%%" ,"%%" ),Singular("%%" ,"%%" );

        (* Always fails *)
        Singular("%d" ,"" ),Singular("%d" ,"%d" );
        Singular("%i" ,"" ),Singular("%i" ,"%i" );
        Singular("%n" ,"" ),Singular("%n" ,"%n" );
        Singular("%N" ,"" ),Singular("%N" ,"%N" );
        Singular("%u" ,"" ),Singular("%u" ,"%u" );
        Singular("%x" ,"" ),Singular("%x" ,"%x" );
        Singular("%X" ,"" ),Singular("%X" ,"%X" );
        Singular("%o" ,"" ),Singular("%o" ,"%o" );
        Singular("%s" ,"" ),Singular("%s" ,"%s" );
        Singular("%S" ,"" ),Singular("%S" ,"%S" );
        Singular("%c" ,"" ),Singular("%c" ,"%c" );
        Singular("%C" ,"" ),Singular("%C" ,"%C" );
        Singular("%f" ,"" ),Singular("%f" ,"%f" );
        Singular("%F" ,"" ),Singular("%F" ,"%F" );
        Singular("%e" ,"" ),Singular("%e" ,"%e" );
        Singular("%E" ,"" ),Singular("%E" ,"%E" );
        Singular("%g" ,"" ),Singular("%g" ,"%g" );
        Singular("%G" ,"" ),Singular("%G" ,"%G" );
        Singular("%B" ,"" ),Singular("%B" ,"%B" );
        Singular("%b" ,"" ),Singular("%b" ,"%b" );
        Singular("%ld","" ),Singular("%ld","%ld");
        Singular("%li","" ),Singular("%li","%li");
        Singular("%lu","" ),Singular("%lu","%lu");
        Singular("%lx","" ),Singular("%lx","%lx");
        Singular("%lX","" ),Singular("%lX","%lX");
        Singular("%lo","" ),Singular("%lo","%lo");
        Singular("%nd","" ),Singular("%nd","%nd");
        Singular("%ni","" ),Singular("%ni","%ni");
        Singular("%nu","" ),Singular("%nu","%nu");
        Singular("%nx","" ),Singular("%nx","%nx");
        Singular("%nX","" ),Singular("%nX","%nX");
        Singular("%no","" ),Singular("%no","%no");
        Singular("%Ld","" ),Singular("%Ld","%Ld");
        Singular("%Li","" ),Singular("%Li","%Li");
        Singular("%Lu","" ),Singular("%Lu","%Lu");
        Singular("%Lx","" ),Singular("%Lx","%Lx");
        Singular("%LX","" ),Singular("%LX","%LX");
        Singular("%Lo","" ),Singular("%Lo","%Lo");
        Singular("%a" ,"" ),Singular("%a" ,"%a" );
        Singular("%t" ,"" ),Singular("%t" ,"%t" );

        (* Mismatch *)
        Singular("%d" ,"%i" ),Singular("%d" ,"%d" );
        Singular("%i" ,"%d" ),Singular("%i" ,"%i" );
        Singular("%n" ,"%d" ),Singular("%n" ,"%n" );
        Singular("%N" ,"%d" ),Singular("%N" ,"%N" );
        Singular("%u" ,"%d" ),Singular("%u" ,"%u" );
        Singular("%x" ,"%d" ),Singular("%x" ,"%x" );
        Singular("%X" ,"%d" ),Singular("%X" ,"%X" );
        Singular("%o" ,"%d" ),Singular("%o" ,"%o" );
        Singular("%s" ,"%d" ),Singular("%s" ,"%s" );
        Singular("%S" ,"%d" ),Singular("%S" ,"%S" );
        Singular("%c" ,"%d" ),Singular("%c" ,"%c" );
        Singular("%C" ,"%d" ),Singular("%C" ,"%C" );
        Singular("%f" ,"%d" ),Singular("%f" ,"%f" );
        Singular("%F" ,"%d" ),Singular("%F" ,"%F" );
        Singular("%e" ,"%d" ),Singular("%e" ,"%e" );
        Singular("%E" ,"%d" ),Singular("%E" ,"%E" );
        Singular("%g" ,"%d" ),Singular("%g" ,"%g" );
        Singular("%G" ,"%d" ),Singular("%G" ,"%G" );
        Singular("%B" ,"%d" ),Singular("%B" ,"%B" );
        Singular("%b" ,"%d" ),Singular("%b" ,"%b" );
        Singular("%ld","%d" ),Singular("%ld","%ld");
        Singular("%li","%d" ),Singular("%li","%li");
        Singular("%lu","%d" ),Singular("%lu","%lu");
        Singular("%lx","%d" ),Singular("%lx","%lx");
        Singular("%lX","%d" ),Singular("%lX","%lX");
        Singular("%lo","%d" ),Singular("%lo","%lo");
        Singular("%nd","%d" ),Singular("%nd","%nd");
        Singular("%ni","%d" ),Singular("%ni","%ni");
        Singular("%nu","%d" ),Singular("%nu","%nu");
        Singular("%nx","%d" ),Singular("%nx","%nx");
        Singular("%nX","%d" ),Singular("%nX","%nX");
        Singular("%no","%d" ),Singular("%no","%no");
        Singular("%Ld","%d" ),Singular("%Ld","%Ld");
        Singular("%Li","%d" ),Singular("%Li","%Li");
        Singular("%Lu","%d" ),Singular("%Lu","%Lu");
        Singular("%Lx","%d" ),Singular("%Lx","%Lx");
        Singular("%LX","%d" ),Singular("%LX","%LX");
        Singular("%Lo","%d" ),Singular("%Lo","%Lo");
        Singular("%a" ,"%d" ),Singular("%a" ,"%a" );
        Singular("%t" ,"%d" ),Singular("%t" ,"%t" );
        Singular("%!" ,"%d" ),Singular("%!" ,"%!" );
        Singular("%%" ,"%d" ),Singular("%%" ,"%%" );

        (* All in one *)
        Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
        ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%", 
        "%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
        ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%"),
        Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
        ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%", 
        "%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
        ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%");
        Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
        ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%", 
        "%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E g %G %B %b %ld %li %lu %lx %lX "
        ^"lo nd ni nu nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%"),
        Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
        ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%", 
        "%d %i %n %N %u %x %X %o %s %S %c %C "
        ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
        ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
        ^"%LX %Lo %a %t %! %%");

        (* Plural forms *)
        Plural("singular %d","plural %i",["%d";"%d"]),Plural("singular %d","singular %d",["%d";"%d"]);
        Plural("singular %d","plural %d",["%i";"%d"]),Plural("singular %d","plural %d",["singular %d";"%d"]);
        Plural("singular %d","plural %d",["%d";"%i"]),Plural("singular %d","plural %d",["%d";"plural %d"]);
        Plural("singular %d","plural %d",["%d";"%d"]),Plural("singular %d","plural %d",["%d";"%d"]);

        (* Idem potent *)
        Singular("%%",""),Singular("%%","");
        Singular("%!",""),Singular("%!","");
        Singular("",""),Singular("","");
        Singular("a","b"),Singular("a","b");
      ]
    )
;;

(**************************)
(* Split plural functions *)
(**************************)

let split_plural_test tests = 
  let split_plural_test_one (str,res_lst) = 
      (Printf.sprintf "Split plural test %S" str) >::
        ( fun () ->
          let lst = GettextUtils.split_plural str
          in
          List.iter2 ( fun str1 str2 -> 
            print_debug tests (Printf.sprintf "Extracted : %S" str2);
            print_debug tests (Printf.sprintf "Expected  : %S" str1);
            if str1 = str2 then 
              ()
            else
              assert_failure (Printf.sprintf "%S should be %S" str2 str1)
          ) res_lst lst
        )
  in
  "Split plural test" >:::
    List.map split_plural_test_one [
      ("%d coffee\000more %d coffee",["%d coffee"; "more %d coffee"])
    ]
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
    fl_mo >::
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
  in
  let install_fail_test_one (fl_mo,exc,error) =
    (fl_mo^" ( "^error^" ) ") >::
      ( fun () ->
        try
          GettextCompile.install tests.test_dir "fr" LC_MESSAGES "gettext-fail" fl_mo;
          assert_failure 
          ("Installation of "^fl_mo^" should have failed with error "^error)
        with x when x = exc ->
          ()
      )
  in
  "MO file installation test" >:::
    (
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
    ) @
    (
      let i32 = Int32.of_int
      in
      List.map install_fail_test_one [
        "test5.mo",GettextMo.InvalidMoFile,
          "MO file invalid ( magic number )";
        "test6.mo",GettextMo.InvalidMoHeaderTableStringOutOfBound((i32 28, i32 2626),(i32 (-1), i32 159)),
          "Offset of table with original strings is out of bound";
        "test7.mo",GettextMo.InvalidMoHeaderTableTranslationOutOfBound((i32 28, i32 2626),(i32 (-49), i32 111)),
          "Offset of table with translation strings is out of bound";
        "test8.mo",GettextMo.InvalidMoStringOutOfBound(2626, 36),
          "Offset of first string is out of bound";
        "test9.mo",GettextMo.InvalidMoTranslationOutOfBound(2626, 196),
          "Offset of first translation is out of bound";
      ]
    )
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

(**********************************)
(* Test of Gettext implementation *)
(**********************************)

let implementation_test tests =
  (* Function for extracting all information of MO file *)
  let extract_parameters fl_mo =
    (* File scheme : 
      base_dir/lang/category/domain.mo
     *)
    let textdomain = 
      chop_extension (basename fl_mo)
    in
    let category =
      GettextCategory.category_of_string (basename (dirname fl_mo))
    in
    let language = 
      (basename (dirname (dirname (fl_mo))))
    in
    let base_dir =
      (dirname (dirname (dirname (fl_mo))))
    in
    let () = 
      print_debug tests ("Filename:   "^fl_mo);
      print_debug tests ("Textdomain: "^textdomain);
      print_debug tests ("Category:   "^(GettextCategory.string_of_category category));
      print_debug tests ("Language:   "^language);
      print_debug tests ("Base dir:   "^base_dir)
    in
    let mo = 
      print_debug tests ("Opening "^fl_mo);
      open_in_bin fl_mo
    in
    let mo_header = 
      print_debug tests "Fetching headers";
      GettextMo.input_mo_header mo
    in
    let mo_informations = 
      print_debug tests "Fetching informations";
      GettextMo.input_mo_informations
      RaiseException mo mo_header
    in
    let test_translations = ref []
    in
    for i = 0 to (Int32.to_int mo_header.number_of_strings) - 1 do
      let translation = 
        print_debug tests ("Fetching translation n°"^(string_of_int i));
        GettextMo.input_mo_translation RaiseException mo mo_header i
      in
      test_translations := translation :: !test_translations
    done;
    print_debug tests ("Closing file "^fl_mo);
    close_in mo;
    (fl_mo,base_dir,language,category,textdomain,!test_translations)
  in
  (* Build the parameter t out of parameters extracted above *)
  let t_of_parameters parameters =
    let (fl_mo,base_dir,language,category,textdomain,test_translations) = 
      parameters
    in
    (* We use a UTF-8 binding, this is the most generic encoding
      for all strings *)
    GettextModules.create 
    ~failsafe:RaiseException
    ~codesets:[(textdomain,"UTF-8")] 
    ~path:[base_dir] 
    ~language:language
    textdomain
  in
  (* Generate a test case of simple load of a MO file using an implementation *)
  let test_load parameters_lst (realize_str,realize) = 
    let test_load_one realize parameters =
      (* Extract usefull information out of parameters *)
      let (fl_mo,_,_,_,_,test_translations) = 
        parameters
      in
      (* Build t *)
      let t =
        t_of_parameters parameters
      in
      (* Build t' *)
      let t' =
        realize t
      in
      let test_one_translation translation =
        (* We cannot compare directly extracted values and t' extracted
           value , since we have a charset translation *)
        try 
          match translation with 
            Singular(str_id,_) ->
              ignore(GettextCompat.gettext t' str_id)
          | Plural(str_id,str_plural,_) ->
              (* Using values from 0 to 2, we cover most of the plural cases *)
              ignore(GettextCompat.ngettext t' str_id str_plural 0);
              ignore(GettextCompat.ngettext t' str_id str_plural 1);
              ignore(GettextCompat.ngettext t' str_id str_plural 2)
        with exc ->
          assert_failure ((Printexc.to_string exc)^" in "
          ^(string_of_translation translation))
      in
      fl_mo >::
        ( fun () ->
          List.iter test_one_translation test_translations
        )
    in
    realize_str >:::
      List.map (test_load_one realize) parameters_lst
  in
  (* Generate a cross test of string extracted, using different implementation *)
  let test_cross implementation_lst parameters = 
    (* Extract usefull information out of parameters *)
    let (fl_mo,_,_,_,_,test_translations) = 
      parameters
    in
    (* Build t *)
    let t =
      t_of_parameters parameters
    in
    (* Build all t' *)
    let t'_lst = 
      List.map 
      (fun (realize_str,realize) -> (realize_str,realize t)) 
      implementation_lst
    in
    let check_translation str lst = 
      let (_,same_str) =
        List.fold_left ( 
          fun (prev_str_opt,res) (_,cur_str) -> 
            match prev_str_opt with
              Some prev_str ->
                (Some cur_str, res && prev_str = cur_str)
            | None ->
                (Some cur_str, res)
          ) (None,true) lst
      in
      if same_str then 
        ()
      else
        assert_failure 
        (
          Printf.sprintf 
          "All values should be identical in [ %s ] in function %s" 
          ( 
            String.concat " ; " 
            (
              List.map ( fun (realize_str,str) ->
                Printf.sprintf "(%s,%S)" realize_str str
              ) lst 
            ) 
          )
          str
        )
    in
    let test_cross_one translation = 
      match translation with
        Singular(str_id,_) ->
          check_translation 
          (
            Printf.sprintf "GettextCompat.gettext t' %S" str_id
          )
          (
            List.map ( 
              fun (realize_str,t') -> 
                (realize_str,GettextCompat.gettext t' str_id)
            ) t'_lst
          )
      | Plural(str_id,str_plural,_) ->
          List.iter ( 
            fun n ->
              check_translation
              (
                Printf.sprintf "GettextCompat.ngettext t' %S %S %d" 
                str_id str_plural n
              )
              (
                List.map ( 
                  fun (realize_str,t') ->
                    (realize_str,GettextCompat.ngettext t' str_id str_plural n)
                ) t'_lst
              )
          ) [ 0 ; 1 ; 2 ]
    in
    fl_mo >::
      ( fun () ->
        List.iter test_cross_one test_translations
      )
  in
  (* Extract and test *)
  let parameters_lst = 
    List.map extract_parameters [
      make_filename ["." ; "fr_FR" ; "LC_MESSAGES" ; "test1.mo" ];
      make_filename ["." ; "fr_FR" ; "LC_MESSAGES" ; "test2.mo" ];
      make_filename ["." ; "fr_FR" ; "LC_MESSAGES" ; "test3.mo" ];
      make_filename ["." ; "fr_FR" ; "LC_MESSAGES" ; "test4.mo" ];
      make_filename ["." ; "fr_FR" ; "LC_MESSAGES" ; "test10.mo" ];
      make_filename ["." ; "fr_FR" ; "LC_MESSAGES" ; "test11.mo" ];
    ]
  in
  let implementation_lst = 
    [
      ("GettextCamomile.Map.realize",     GettextCamomile.Map.realize);
      ("GettextCamomile.Hashtbl.realize", GettextCamomile.Hashtbl.realize);
      ("GettextCamomile.Open.realize",    GettextCamomile.Open.realize);
      ("GettextStub.Native.realize",      GettextStub.Native.realize);
      ("GettextStub.Preload.realize",     GettextStub.Preload.realize);
    ]
  in
  "Gettext implementation test" >:::
    [
      "Load" >:::
        List.map (test_load parameters_lst) implementation_lst;
      "Cross check" >:::
        List.map (test_cross implementation_lst) parameters_lst;
    ]
;;

(*********************)
(* Main test routine *)
(*********************)

let tests = parse_arg ()
in
let all_test = 
  "Test ocaml-gettext" >::: 
    [
      format_test         tests;
      split_plural_test   tests;
      po_test             tests; 
      compatibility_test  tests;
      extract_test        tests;
      install_test        tests;
      implementation_test tests;
      (* BUG : to reenable when releasing v 0.3 *)
      (*merge_test         tests;*)
    ]
in
let () = 
  print_endline ("Test            : ocaml-gettext "^(GettextConfig.version));
  print_endline ("Test build date : "^(GettextConfig.build_date));
  print_endline ("OS              : "^(Sys.os_type));
  print_endline ("Test dir        : "^(tests.test_dir));
  mkdir ~parent:true tests.test_dir;
  print_endline ("Running...")
in
run_test_tt all_test

