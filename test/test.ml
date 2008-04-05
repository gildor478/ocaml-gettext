(**************************************************************************)
(*  Ocaml-gettext : a library to translate messages                       *)
(*                                                                        *)
(*  Copyright (C) 2003, 2004, 2005 Sylvain Le Gall <sylvain@le-gall.net>  *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(*                                                                        *)
(*  Contact: sylvain@le-gall.net                                          *)
(**************************************************************************)

open OUnit;;
open FileUtil;;
(* BUG: should be independant of Str *)
open FileUtil.StrUtil;;
open FilePath;;
open FilePath.DefaultPath;;
open GettextTypes;;
open GettextCategory;;
open Common;;

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
    ocaml_xgettext = make_filename [ parent_dir ; "_build" ; "bin" ; "ocaml-xgettext"];
    test_dir       = make_filename [ current_dir ; "tests" ];
  }
  in
  Arg.parse ( Arg.align [
    (
      "--search", Arg.String ( 
        fun dir -> 
          tests := { !tests with search_path = dir :: !tests.search_path }
        )
      ,"dir Search the specified directory for MO file."
    );
    (
      "--verbose", Arg.Unit (
        fun () ->
          tests := { !tests with verbose = true }
        )
      ,"Processs with a lot of message."
    );
    (
      "--ocaml-xgettext", Arg.String (
        fun s ->
          tests := { !tests with ocaml_xgettext = s }
        )
      ,"cmd Specify the ocaml-xgettext executable."
    );
    (
      "--test-dir", Arg.String (
        fun s ->
          tests := { !tests with test_dir = s }
        )
      ,"dir Specify the temporary dir for testing files."
    );
  ])
  (fun str -> ())
  ("Test utility for ocaml-gettext v"^(GettextConfig.version)^" by Sylvain Le Gall\n"^
  "Copyright 2004,2005. Licensed under LGPL v2.1 with Ocaml exception.");
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
        let mo_header = GettextMo.input_mo_header mo
        in
        print_debug tests (GettextMo.string_of_mo_header mo_header);
        close_in mo
      with x ->
        assert_failure (fl_mo^" doesn't load properly: "^(Gettext.string_of_exception x))
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
        assert_failure (fl_mo^" doesn't load properly: "^(Gettext.string_of_exception x))
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
          assert_failure (fl_po^" doesn't parse correctly: "^(Gettext.string_of_exception x))
      );

      "Compiling" >::
      ( fun () ->
        try
          let _ = GettextCompile.compile fl_po fl_mo
          in
          () 
        with x ->
          assert_failure (fl_po^" doesn't compile correctly"^(Gettext.string_of_exception x))
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
      List.map format_test_one format_translation_check_data
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
              assert_failure (fl_ml^" doesn't extract correctly: "^(Gettext.string_of_exception x))
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
        "test5.mo",MoInvalidFile,
          "MO file invalid ( magic number )";
        "test6.mo",MoInvalidHeaderTableStringOutOfBound((i32 28, i32 2626),(i32 (-1), i32 159)),
          "Offset of table with original strings is out of bound";
        "test7.mo",MoInvalidHeaderTableTranslationOutOfBound((i32 28, i32 2626),(i32 (-49), i32 111)),
          "Offset of table with translation strings is out of bound";
        "test8.mo",MoInvalidStringOutOfBound(2626, 36),
          "Offset of first string is out of bound";
        "test9.mo",MoInvalidTranslationOutOfBound(2626, 196),
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
  (* Generate a test case of simple load of a MO file using an implementation *)
  let test_load parameters_lst (realize_str,realize) = 
    let test_load_one realize parameters =
      (* Extract usefull information out of parameters *)
      let fl_mo = 
        parameters.fl_mo
      in
      let test_translations = 
        parameters.translations
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
    let fl_mo = 
      parameters.fl_mo
    in
    let test_translations= 
      parameters.translations
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
    List.map parameters_of_filename mo_files_data 
  in
  let implementation_lst = 
    realize_data
  in
  "Gettext implementation test" >:::
    [
      "Load" >:::
        List.map (test_load parameters_lst) implementation_lst;
      "Cross check" >:::
        List.map (test_cross implementation_lst) parameters_lst;
    ]
;;

(******************************)
(* Test for multiline comment *)
(******************************)

let multiline_comment_test tests = 
  "Gettext multiline comment test" >:::
  [
    "multinline-comment.po" >:: 
    (
      fun () ->
        ignore(
          let src_po =
            make_filename [current_dir; "multiline-comment.po"]
          in
          let tgt_po =
            make_filename [tests.test_dir; "multiline-comment.po"]
          in
          let tgt_po_bak =
            make_filename [tests.test_dir; "multiline-comment.po.bak"]
          in
            cp [src_po] tgt_po;
            GettextCompile.merge tgt_po [tgt_po] "bak";
            match cmp tgt_po tgt_po_bak with
              | Some -1 -> 
                  assert_failure (tgt_po^" or "^tgt_po_bak^" doesn't exist")
              | Some x ->
                  assert_failure (tgt_po^" differs from "^tgt_po_bak)
              | None ->
                  ()
        )
    )
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
      format_test            tests;
      split_plural_test      tests;
      po_test                tests; 
      compatibility_test     tests;
      extract_test           tests;
      install_test           tests;
      implementation_test    tests;
      multiline_comment_test tests;
      (* BUG : to reenable when releasing v 0.3 *)
      (*merge_test           tests;*)
    ]
in
print_env "tests";
mkdir ~parent:true tests.test_dir;
run_test_tt all_test

