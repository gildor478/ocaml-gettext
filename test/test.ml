(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
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
(**************************************************************************)

open OUnit;;
open FileUtil;;
open FilePath;;
open GettextTypes;;
open GettextCategory;;
open Common;;

type tests = {
  verbose        : bool;
  search_path    : string list;
  ocaml_xgettext : string;
  ocaml_gettext  : string;
  test_dir       : string;
}
;;

let parse_arg () = 
  let tests = ref { 
    verbose        = false; 
    search_path    = []; 
    ocaml_xgettext = make_filename [ parent_dir ; "_build" ; "bin" ; "ocaml-xgettext"];
    ocaml_gettext  = make_filename [ parent_dir ; "_build" ; "bin" ; "ocaml-gettext"];
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
      "--ocaml-gettext", Arg.String (
        fun s ->
          tests := { !tests with ocaml_gettext = s }
        )
      ,"cmd Specify the ocaml-gettext executable."
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
   "Copyright (C) 2004-2008 Sylvain Le Gall <sylvain@le-gall.net>\n"^
   "Licensed under LGPL v2.1 with OCaml exception.");
  !tests
;;

let print_debug tests str =
  if tests.verbose then
    (print_string str; print_newline ())
  else
    ()
;;

let run_and_read prog ?(env=[||]) cli = 

  (* Temporary file to retain data from command *)
  let fn_out, chn_out = 
    Filename.open_temp_file "ocaml-gettext-out" ".txt"
  in
  let fn_err, chn_err = 
    Filename.open_temp_file "ocaml-gettext-err" ".txt"
  in

  (* Clean after run *)
  let cleanup () =
    let safe f a =
      try 
        f a 
      with _ ->
        ()
    in
      safe close_out chn_out; safe close_out chn_err;
      safe Sys.remove fn_out; safe Sys.remove fn_err;
  in

  let input_fn fn = 
    let chn_in =
      open_in fn
    in
    let buff =
      Buffer.create 13
    in
      Buffer.add_channel buff chn_in (in_channel_length chn_in);
      close_in chn_in;
      Buffer.contents buff
  in

    try 
      let stdin_in, stdin_out = 
        Unix.pipe ()
      in

      let command_array =
          (Array.of_list (prog :: cli))
      in
      let command = 
        String.concat " " (Array.to_list command_array)
      in

      let pid = 
        Unix.create_process_env 
          prog
          command_array
          env
          stdin_in
          (Unix.descr_of_out_channel chn_out)
          (Unix.descr_of_out_channel chn_err)
      in
      let () = 
        Unix.close stdin_in; Unix.close stdin_out
      in
      let return_code =
        match snd (Unix.waitpid [] pid) with 
          | Unix.WEXITED code 
          | Unix.WSIGNALED code 
          | Unix.WSTOPPED code ->
              code
      in
      let err =
        close_out chn_err;
        input_fn fn_err
      in
      let out =
        close_out chn_out;
        input_fn fn_out
      in
        cleanup ();
        command, return_code, out, err
    with e ->
      (
        cleanup ();
        raise e
      )
;;

let load_mo_file tests f_test_mo fl_mo =
  let mo = 
    open_in_bin fl_mo
  in
  let mo_header = 
    GettextMo.input_mo_header mo
  in
  let mo_informations = 
    GettextMo.input_mo_informations
      RaiseException 
      mo 
      mo_header
  in
    print_debug 
      tests 
      (GettextMo.string_of_mo_header mo_header);
    print_debug 
      tests 
      (GettextMo.string_of_mo_informations mo_informations);
    close_in mo;
    f_test_mo fl_mo 
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
  let po_test_one f_test_mo fl_po = 
    let fl_mo = 
      concat 
        tests.test_dir 
        (replace_extension fl_po "mo")
    in
      (Printf.sprintf "Load and compile %s" fl_po) >:: 
        (fun () ->
           let chn = 
             open_in fl_po
           in
             ignore (GettextPo.input_po chn);
             close_in chn;
             GettextCompile.compile fl_po fl_mo;
             load_mo_file tests f_test_mo fl_mo)
  in
    "PO processing test" >:::
      (List.map (po_test_one ignore)
         [
           "test1.po"; 
           "test2.po"; 
           "test3.po"; 
           "test4.po";
           "test11.po";
         ])
    @
    [
      po_test_one 
        (fun fl_mo ->
           let (), _ = 
             GettextMo.fold_mo
               RaiseException 
               (fun trslt () ->
                  match trslt with 
                    | Singular(id, "") 
                    | Plural(id, _, []) 
                    | Plural(id, _, [""]) ->
                        assert_failure
                          (Printf.sprintf 
                             "%s contains an empty translation for %s"
                             fl_mo
                             id)
                    | _ ->
                        ()) 
                 ()
                 fl_mo
           in 
             ())
        "test12.po"
    ]
;;




(****************************************************)
(* Test compatibility with already produced mo file *)
(****************************************************)

let compatibility_test tests =
  let test_one_mo fl =
    (Printf.sprintf "Loading %s" fl) >:: 
     (fun () -> load_mo_file tests ignore fl)
  in
    "Test compatibility" >:::
      List.fold_left 
        (fun lst dir -> 
           find 
             (Has_extension "mo") 
             dir 
             (fun lst fln -> (test_one_mo fln) :: lst) 
             lst) 
        [] 
        tests.search_path
;;

(*******************************************)
(* Test of OCaml source file PO extraction *)
(*******************************************)

let extract_test tests = 
  let default_options = 
    "-I +camlp4 pa_o.cmo"
  in
  let filename_options = 
    MapString.empty
  in
  let extract_test_one (fl_ml, contents) = 
    let fl_pot = 
      concat tests.test_dir (replace_extension fl_ml "pot")
    in
    fl_ml >:::
      [ 
        "Extracting" >::
          (fun () ->
             (
               (* Extract data from files *)
               try
                 GettextCompile.extract 
                   tests.ocaml_xgettext 
                   default_options 
                   filename_options 
                   [fl_ml] 
                   fl_pot
               with x ->
                 assert_failure 
                   (fl_ml^" doesn't extract correctly: "^
                    (Gettext.string_of_exception x))
             );
             (* Load POT file *) 
             let po = 
                let chn = 
                  open_in fl_pot
                in
                let res = 
                  GettextPo.input_po chn
                in
                  close_in chn;
                  res
             in

             (* Check result *)
             List.iter 
               (fun str ->
                  if MapString.mem str po.no_domain then
                    ()
                  else
                    assert_failure
                      (Printf.sprintf 
                         "Cannot find %S in %s"
                         str
                         fl_pot))
               contents
          )
      ]
  in
  "OCaml file extraction test" >:::
    List.map extract_test_one 
      [ 
        "test4.ml", [];
        "escape-char.ml", ["hello world!\n"; 
                           "goodbye world!\n"; 
                           "goodby world 2!"; 
                           "and then\tbye-bye"];
      ]
;;

(********************************)
(* Test of MO file installation *)
(********************************)

let install_test tests =
  let install_test_one (language, category, textdomain, fl_mo, fl_dsts) =  
    fl_mo >::
      (fun () ->
         let fl_dst = 
           make_filename fl_dsts
         in
           GettextCompile.install 
             true 
             tests.test_dir 
             language 
             category 
             textdomain 
             fl_mo;
          assert_bool
            (Printf.sprintf "%s is not installed at %s" fl_mo fl_dst)
            (test Exists fl_dst))
  in

  let install_fail_test_one (fl_mo, exc) =
    let  error = 
      Printexc.to_string exc
    in
      (Printf.sprintf "%s (%s)" fl_mo error) >::
        (fun () ->
           assert_raises
            ~msg:(Printf.sprintf 
                    "Installation of %s should have failed with error %s" 
                    fl_mo error)
             exc
             (fun () ->
                GettextCompile.install
                  true 
                  tests.test_dir 
                  "fr" 
                  LC_MESSAGES 
                  "gettext-fail"
                  fl_mo))
  in

  let install_warning_test_one (language, category, textdomain, fl_mo, exp_err, fl_dsts) =  
    (Printf.sprintf "%s warning" fl_mo) >::
     (fun () ->
        let command, return_code, out, err = 
          run_and_read 
            tests.ocaml_gettext
            [
              "--action"; "install";
              "--install-language"; language;
              "--install-category"; category;
              "--install-textdomain"; textdomain;
              "--install-destdir"; tests.test_dir;
              fl_mo
            ]
        in
        let fl_dst =
          make_filename fl_dsts
        in
          assert_equal 
            ~msg:(command^" return code")
            ~printer:string_of_int
            0
            return_code;
          assert_equal
            ~msg:(command^" standard output")
            ~printer:(Printf.sprintf "%S")
            ""
            out;
          assert_equal
            ~msg:(command^" error output")
            ~printer:(Printf.sprintf "%S")
            exp_err
            err;
          assert_bool
            (Printf.sprintf "File %s doesn't exist" fl_dst)
            (test Exists fl_dst))
  in

    "MO file installation test" >:::
      (List.map install_test_one 
         [
           "fr",LC_MESSAGES, "gettext-test1", concat tests.test_dir "test1.mo", 
           [tests.test_dir; "fr"; "LC_MESSAGES"; "gettext-test1.mo"];

           "fr_FR",LC_MESSAGES, "gettext-test2", concat tests.test_dir "test2.mo", 
           [tests.test_dir; "fr_FR"; "LC_MESSAGES"; "gettext-test2.mo"];

           "fr",LC_TIME, "gettext-test3", concat tests.test_dir "test3.mo", 
           [tests.test_dir; "fr"; "LC_TIME"; "gettext-test3.mo"];

           "fr_FR@euro",LC_MESSAGES, "gettext-test4", concat tests.test_dir "test4.mo",
           [tests.test_dir; "fr_FR@euro"; "LC_MESSAGES"; "gettext-test4.mo"];
         ]) @
      (List.map install_fail_test_one 
         [
           "test5.mo", 
           MoInvalidFile;

           "test6.mo", 
           MoInvalidHeaderTableStringOutOfBound((28l, 2626l),(-1l, 159l));

           "test7.mo", 
           MoInvalidHeaderTableTranslationOutOfBound((28l, 2626l),(-49l, 111l));

           "test8.mo", 
           MoInvalidStringOutOfBound(2626, 36);

           "test9.mo", 
           MoInvalidTranslationOutOfBound(2626, 196);
         ]) @
      (List.map install_warning_test_one
         [
           "fr", "LC_MESSAGES", "test10", "test10.mo", 
           "Error while processing parsing of plural at line 1 character 10: \" nplurals=INTEGER; plural=EXPRESSION;\".\n",
           [tests.test_dir; "fr"; "LC_MESSAGES"; "test10.mo"];
         ])
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
                add_extension fl_po_cp backup_ext
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
    List.map merge_one [ (concat tests.test_dir "test4.pot","test4.po","bak") ]
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

(**********************************)
(* Test for PO processing comment *)
(**********************************)

let po_process_test tests =
  let copy_merge_compare fn_po =
    let src_po =
      make_filename [current_dir; fn_po]
    in
    let tgt_po =
      make_filename [tests.test_dir; fn_po]
    in
      cp [src_po] tgt_po;
      GettextCompile.merge tgt_po [tgt_po] "bak";
      match cmp tgt_po src_po with
        | Some -1 -> 
            assert_failure (tgt_po^" or "^src_po^" doesn't exist")
        | Some x ->
            assert_failure (tgt_po^" differs from "^src_po)
        | None ->
            ()
  in
  "Gettext po process test" >:::
  [
    "multiline-comment.po" >:: 
    (
      fun () ->
        copy_merge_compare "multiline-comment.po"
    );
    "utf8-fr.po" >::
    (
      fun () ->
        copy_merge_compare "utf8-fr.po"
    );
    "utf8-ja.po" >::
    (
      fun () ->
        copy_merge_compare "utf8-ja.po"
    );
  ]
;;

(********************************************)
(* Test for running ocaml-gettext program   *)
(* (spot also problem with runtime behavior *)
(* of ocaml-gettext library)                *)
(********************************************)

let run_ocaml_gettext tests =
  "Running ocaml-gettext" >:::
  [
    "ocaml-gettext with LC_ALL and LANG unset" >::
    (fun () ->
       let command, return_code, _, _ = 
         run_and_read 
           tests.ocaml_gettext
           ["--help"]
       in
         assert_equal
           ~msg:("return code of "^command)
           ~printer:string_of_int
           0
           return_code)
  ]
;;


(************************************************)
(* Set bad locale and spot error when setlocale *)
(* returns NULL                                 *)
(************************************************)

let bad_setlocale tests =
  "Call setlocale with bad locale" >:::
  [
    "setlocale with bad locale" >::
    (
      fun () ->
	ignore (GettextStubCompat.setlocale GettextStubCompat.LC_ALL "xx");
    )
  ]
;;

(******************)
(* Try to compile *)
(******************)

let compile_ocaml _ = 
  "Compile OCaml code" >:::
  (List.map 
     (fun (fn, exp_return_code, exp_out, exp_err) ->
        fn >::
        (fun () ->
           let command, return_code, out, err = 
             run_and_read 
               "ocamlc" 
               ["-c"; "-I"; "../libgettext-ocaml/"; "-I"; "../libgettext-stub-ocaml/"; "TestGettext.ml"; fn]
           in
             FileUtil.rm (List.map (FilePath.replace_extension fn) ["cmo"; "cmi"]);
             FileUtil.rm (List.map (FilePath.add_extension "TestGettext") ["cmo"; "cmi"]);
             assert_equal 
               ~msg:("error output of "^command)
               ~printer:(Printf.sprintf "%S")
               exp_err
               err;
             assert_equal 
               ~msg:("standard output of "^command)
               ~printer:(Printf.sprintf "%S")
               exp_out
               out;
             assert_equal 
               ~msg:("return code of "^command)
               ~printer:string_of_int
               exp_return_code
               return_code))
     [
       "unsound_warning.ml", 0, "", "";
       "valid_format.ml", 0, "", "";
       "invalid_format1.ml", 2, "",
       "File \"invalid_format1.ml\", line 5, characters 21-22:\n\
        Error: This expression has type int but an expression was expected of type\n\
       \         int32\n";
       "invalid_format2.ml", 2, "", 
       "File \"invalid_format2.ml\", line 5, characters 21-22:\n\
        Error: This expression has type int but an expression was expected of type\n\
       \         int64\n";
       "invalid_format3.ml", 2, "",
       "File \"invalid_format3.ml\", line 5, characters 20-21:\n\
        Error: This expression has type int but an expression was expected of type\n\
       \         string\n";
       "invalid_format4.ml", 2, "",
       "File \"invalid_format4.ml\", line 5, characters 20-21:\n\
        Error: This expression has type int but an expression was expected of type\n\
       \         bool\n";
       "invalid_format5.ml", 2, "",
       "File \"invalid_format5.ml\", line 5, characters 29-45:\n\
        Error: This expression has type (int64 -> 'a, 'b, 'c, 'd, 'd, 'a) format6\n\
       \       but an expression was expected of type\n\
       \         (int -> 'e, 'f, 'g, 'g, 'g, 'e) format6\n";
     ])
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
      po_process_test        tests;
      merge_test             tests;
      run_ocaml_gettext      tests;
      bad_setlocale          tests;
      compile_ocaml          tests;
    ]
in
print_env "tests";
mkdir ~parent:true tests.test_dir;
run_test_tt all_test

