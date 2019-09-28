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

open GettextTypes
open GettextCategory
open FilePath.DefaultPath
open OUnit

(* Version data *)
let print_env str =
  print_endline ("Version         : ocaml-gettext " ^ GettextConfig.version);
  print_endline ("OS              : " ^ Sys.os_type);
  print_endline ("Running " ^ str ^ " ...")

(* Print a translation *)
let string_of_translation trans =
  match trans with
  | Singular (str_id, str) -> Printf.sprintf "Singular(%S, %S)" str_id str
  | Plural (str_id, str_plural, lst) ->
      Printf.sprintf "Plural(%S, %S, [ %s ])" str_id str_plural
        (String.concat " ; " (List.map (fun x -> Printf.sprintf "%S" x) lst))

(* Function for extracting all information of MO file *)
type parameters = {
  fl_mo : filename;
  base_dir : dir;
  language : locale;
  category : category;
  textdomain : textdomain;
  translations : translation list;
}

let parameters_of_filename fl_mo =
  (* File scheme:
    base_dir/lang/category/domain.mo
   *)
  let textdomain = chop_extension (basename fl_mo) in
  let category =
    GettextCategory.category_of_string (basename (dirname fl_mo))
  in
  let language = basename (dirname (dirname fl_mo)) in
  let base_dir = dirname (dirname (dirname fl_mo)) in
  let translations, _ =
    GettextMo.fold_mo RaiseException (fun x lst -> x :: lst) [] fl_mo
  in
  { fl_mo; base_dir; language; category; textdomain; translations }

(* Build the parameter t out of parameters extracted above *)
let t_of_parameters parameters =
  (* We use a UTF-8 binding, this is the most generic encoding
  * for all strings *)
  GettextModules.create ~failsafe:RaiseException
    ~codesets:[ (parameters.textdomain, "UTF-8") ]
    ~path:[ parameters.base_dir ] ~language:parameters.language
    parameters.textdomain

(* Data for format test/bench *)
let format_translation_check_data =
  [
    (* Identity *)
      (Singular ("%d", "%d"), Singular ("%d", "%d"));
    (Singular ("%i", "%i"), Singular ("%i", "%i"));
    (Singular ("%n", "%n"), Singular ("%n", "%n"));
    (Singular ("%N", "%N"), Singular ("%N", "%N"));
    (Singular ("%u", "%u"), Singular ("%u", "%u"));
    (Singular ("%x", "%x"), Singular ("%x", "%x"));
    (Singular ("%X", "%X"), Singular ("%X", "%X"));
    (Singular ("%o", "%o"), Singular ("%o", "%o"));
    (Singular ("%s", "%s"), Singular ("%s", "%s"));
    (Singular ("%S", "%S"), Singular ("%S", "%S"));
    (Singular ("%c", "%c"), Singular ("%c", "%c"));
    (Singular ("%C", "%C"), Singular ("%C", "%C"));
    (Singular ("%f", "%f"), Singular ("%f", "%f"));
    (Singular ("%F", "%F"), Singular ("%F", "%F"));
    (Singular ("%e", "%e"), Singular ("%e", "%e"));
    (Singular ("%E", "%E"), Singular ("%E", "%E"));
    (Singular ("%g", "%g"), Singular ("%g", "%g"));
    (Singular ("%G", "%G"), Singular ("%G", "%G"));
    (Singular ("%B", "%B"), Singular ("%B", "%B"));
    (Singular ("%b", "%b"), Singular ("%b", "%b"));
    (Singular ("%ld", "%ld"), Singular ("%ld", "%ld"));
    (Singular ("%li", "%li"), Singular ("%li", "%li"));
    (Singular ("%lu", "%lu"), Singular ("%lu", "%lu"));
    (Singular ("%lx", "%lx"), Singular ("%lx", "%lx"));
    (Singular ("%lX", "%lX"), Singular ("%lX", "%lX"));
    (Singular ("%lo", "%lo"), Singular ("%lo", "%lo"));
    (Singular ("%nd", "%nd"), Singular ("%nd", "%nd"));
    (Singular ("%ni", "%ni"), Singular ("%ni", "%ni"));
    (Singular ("%nu", "%nu"), Singular ("%nu", "%nu"));
    (Singular ("%nx", "%nx"), Singular ("%nx", "%nx"));
    (Singular ("%nX", "%nX"), Singular ("%nX", "%nX"));
    (Singular ("%no", "%no"), Singular ("%no", "%no"));
    (Singular ("%Ld", "%Ld"), Singular ("%Ld", "%Ld"));
    (Singular ("%Li", "%Li"), Singular ("%Li", "%Li"));
    (Singular ("%Lu", "%Lu"), Singular ("%Lu", "%Lu"));
    (Singular ("%Lx", "%Lx"), Singular ("%Lx", "%Lx"));
    (Singular ("%LX", "%LX"), Singular ("%LX", "%LX"));
    (Singular ("%Lo", "%Lo"), Singular ("%Lo", "%Lo"));
    (Singular ("%a", "%a"), Singular ("%a", "%a"));
    (Singular ("%t", "%t"), Singular ("%t", "%t"));
    (Singular ("%!", "%!"), Singular ("%!", "%!"));
    (Singular ("%%", "%%"), Singular ("%%", "%%"));
    (* Always fails *)
      (Singular ("%d", ""), Singular ("%d", "%d"));
    (Singular ("%i", ""), Singular ("%i", "%i"));
    (Singular ("%n", ""), Singular ("%n", "%n"));
    (Singular ("%N", ""), Singular ("%N", "%N"));
    (Singular ("%u", ""), Singular ("%u", "%u"));
    (Singular ("%x", ""), Singular ("%x", "%x"));
    (Singular ("%X", ""), Singular ("%X", "%X"));
    (Singular ("%o", ""), Singular ("%o", "%o"));
    (Singular ("%s", ""), Singular ("%s", "%s"));
    (Singular ("%S", ""), Singular ("%S", "%S"));
    (Singular ("%c", ""), Singular ("%c", "%c"));
    (Singular ("%C", ""), Singular ("%C", "%C"));
    (Singular ("%f", ""), Singular ("%f", "%f"));
    (Singular ("%F", ""), Singular ("%F", "%F"));
    (Singular ("%e", ""), Singular ("%e", "%e"));
    (Singular ("%E", ""), Singular ("%E", "%E"));
    (Singular ("%g", ""), Singular ("%g", "%g"));
    (Singular ("%G", ""), Singular ("%G", "%G"));
    (Singular ("%B", ""), Singular ("%B", "%B"));
    (Singular ("%b", ""), Singular ("%b", "%b"));
    (Singular ("%ld", ""), Singular ("%ld", "%ld"));
    (Singular ("%li", ""), Singular ("%li", "%li"));
    (Singular ("%lu", ""), Singular ("%lu", "%lu"));
    (Singular ("%lx", ""), Singular ("%lx", "%lx"));
    (Singular ("%lX", ""), Singular ("%lX", "%lX"));
    (Singular ("%lo", ""), Singular ("%lo", "%lo"));
    (Singular ("%nd", ""), Singular ("%nd", "%nd"));
    (Singular ("%ni", ""), Singular ("%ni", "%ni"));
    (Singular ("%nu", ""), Singular ("%nu", "%nu"));
    (Singular ("%nx", ""), Singular ("%nx", "%nx"));
    (Singular ("%nX", ""), Singular ("%nX", "%nX"));
    (Singular ("%no", ""), Singular ("%no", "%no"));
    (Singular ("%Ld", ""), Singular ("%Ld", "%Ld"));
    (Singular ("%Li", ""), Singular ("%Li", "%Li"));
    (Singular ("%Lu", ""), Singular ("%Lu", "%Lu"));
    (Singular ("%Lx", ""), Singular ("%Lx", "%Lx"));
    (Singular ("%LX", ""), Singular ("%LX", "%LX"));
    (Singular ("%Lo", ""), Singular ("%Lo", "%Lo"));
    (Singular ("%a", ""), Singular ("%a", "%a"));
    (Singular ("%t", ""), Singular ("%t", "%t"));
    (* Mismatch *)
      (Singular ("%d", "%i"), Singular ("%d", "%d"));
    (Singular ("%i", "%d"), Singular ("%i", "%i"));
    (Singular ("%n", "%d"), Singular ("%n", "%n"));
    (Singular ("%N", "%d"), Singular ("%N", "%N"));
    (Singular ("%u", "%d"), Singular ("%u", "%u"));
    (Singular ("%x", "%d"), Singular ("%x", "%x"));
    (Singular ("%X", "%d"), Singular ("%X", "%X"));
    (Singular ("%o", "%d"), Singular ("%o", "%o"));
    (Singular ("%s", "%d"), Singular ("%s", "%s"));
    (Singular ("%S", "%d"), Singular ("%S", "%S"));
    (Singular ("%c", "%d"), Singular ("%c", "%c"));
    (Singular ("%C", "%d"), Singular ("%C", "%C"));
    (Singular ("%f", "%d"), Singular ("%f", "%f"));
    (Singular ("%F", "%d"), Singular ("%F", "%F"));
    (Singular ("%e", "%d"), Singular ("%e", "%e"));
    (Singular ("%E", "%d"), Singular ("%E", "%E"));
    (Singular ("%g", "%d"), Singular ("%g", "%g"));
    (Singular ("%G", "%d"), Singular ("%G", "%G"));
    (Singular ("%B", "%d"), Singular ("%B", "%B"));
    (Singular ("%b", "%d"), Singular ("%b", "%b"));
    (Singular ("%ld", "%d"), Singular ("%ld", "%ld"));
    (Singular ("%li", "%d"), Singular ("%li", "%li"));
    (Singular ("%lu", "%d"), Singular ("%lu", "%lu"));
    (Singular ("%lx", "%d"), Singular ("%lx", "%lx"));
    (Singular ("%lX", "%d"), Singular ("%lX", "%lX"));
    (Singular ("%lo", "%d"), Singular ("%lo", "%lo"));
    (Singular ("%nd", "%d"), Singular ("%nd", "%nd"));
    (Singular ("%ni", "%d"), Singular ("%ni", "%ni"));
    (Singular ("%nu", "%d"), Singular ("%nu", "%nu"));
    (Singular ("%nx", "%d"), Singular ("%nx", "%nx"));
    (Singular ("%nX", "%d"), Singular ("%nX", "%nX"));
    (Singular ("%no", "%d"), Singular ("%no", "%no"));
    (Singular ("%Ld", "%d"), Singular ("%Ld", "%Ld"));
    (Singular ("%Li", "%d"), Singular ("%Li", "%Li"));
    (Singular ("%Lu", "%d"), Singular ("%Lu", "%Lu"));
    (Singular ("%Lx", "%d"), Singular ("%Lx", "%Lx"));
    (Singular ("%LX", "%d"), Singular ("%LX", "%LX"));
    (Singular ("%Lo", "%d"), Singular ("%Lo", "%Lo"));
    (Singular ("%a", "%d"), Singular ("%a", "%a"));
    (Singular ("%t", "%d"), Singular ("%t", "%t"));
    (Singular ("%!", "%d"), Singular ("%!", "%!"));
    (Singular ("%%", "%d"), Singular ("%%", "%%"));
    (* All in one *)
      ( Singular
          ( "%d %i %n %N %u %x %X %o %s %S %c %C "
            ^ "%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
            ^ "%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
            ^ "%LX %Lo %a %t %! %%",
            "%d %i %n %N %u %x %X %o %s %S %c %C "
            ^ "%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
            ^ "%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
            ^ "%LX %Lo %a %t %! %%" ),
        Singular
          ( "%d %i %n %N %u %x %X %o %s %S %c %C "
            ^ "%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
            ^ "%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
            ^ "%LX %Lo %a %t %! %%",
            "%d %i %n %N %u %x %X %o %s %S %c %C "
            ^ "%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
            ^ "%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
            ^ "%LX %Lo %a %t %! %%" ) );
    ( Singular
        ( "%d %i %n %N %u %x %X %o %s %S %c %C "
          ^ "%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
          ^ "%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
          ^ "%LX %Lo %a %t %! %%",
          "%d %i %n %N %u %x %X %o %s %S %c %C "
          ^ "%f %F %e %E g %G %B %b %ld %li %lu %lx %lX "
          ^ "lo nd ni nu nx %nX %no %Ld %Li %Lu %Lx " ^ "%LX %Lo %a %t %! %%"
        ),
      Singular
        ( "%d %i %n %N %u %x %X %o %s %S %c %C "
          ^ "%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
          ^ "%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
          ^ "%LX %Lo %a %t %! %%",
          "%d %i %n %N %u %x %X %o %s %S %c %C "
          ^ "%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
          ^ "%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
          ^ "%LX %Lo %a %t %! %%" ) );
    (* Plural forms *)
      ( Plural ("singular %d", "plural %i", [ "%d"; "%d" ]),
        Plural ("singular %d", "singular %d", [ "%d"; "%d" ]) );
    ( Plural ("singular %d", "plural %d", [ "%i"; "%d" ]),
      Plural ("singular %d", "plural %d", [ "singular %d"; "%d" ]) );
    ( Plural ("singular %d", "plural %d", [ "%d"; "%i" ]),
      Plural ("singular %d", "plural %d", [ "%d"; "plural %d" ]) );
    ( Plural ("singular %d", "plural %d", [ "%d"; "%d" ]),
      Plural ("singular %d", "plural %d", [ "%d"; "%d" ]) );
    (* Idem potent *)
      (Singular ("%%", ""), Singular ("%%", ""));
    (Singular ("%!", ""), Singular ("%!", ""));
    (Singular ("", ""), Singular ("", ""));
    (Singular ("a", "b"), Singular ("a", "b"));
  ]

let format_translation_all_data =
  List.fold_left
    (fun lst (a, b) -> a :: b :: lst)
    [] format_translation_check_data

let format_translation_plural_data =
  List.filter
    (function Plural (_, _, _) -> true | _ -> false)
    format_translation_all_data

let format_translation_singular_data =
  List.filter
    (function Singular (_, _) -> true | _ -> false)
    format_translation_all_data

(* Files installed for testing purpose. "." refers to the directory
   where common.ml is installed. *)
let mo_files_data testdata_dir =
  [
    make_filename [ testdata_dir; "fr_FR"; "LC_MESSAGES"; "test1.mo" ];
    make_filename [ testdata_dir; "fr_FR"; "LC_MESSAGES"; "test2.mo" ];
    make_filename [ testdata_dir; "fr_FR"; "LC_MESSAGES"; "test3.mo" ];
    make_filename [ testdata_dir; "fr_FR"; "LC_MESSAGES"; "test4.mo" ];
    make_filename [ testdata_dir; "fr_FR"; "LC_MESSAGES"; "test10.mo" ];
    make_filename [ testdata_dir; "fr_FR"; "LC_MESSAGES"; "test11.mo" ];
  ]

let run_and_read prog ?(env = [||]) cli =
  (* Temporary file to retain data from command *)
  let fn_out, chn_out = Filename.open_temp_file "ocaml-gettext-out" ".txt" in
  let fn_err, chn_err = Filename.open_temp_file "ocaml-gettext-err" ".txt" in
  (* Clean after run *)
  let cleanup () =
    let safe f a = try f a with _ -> () in
    safe close_out chn_out;
    safe close_out chn_err;
    safe Sys.remove fn_out;
    safe Sys.remove fn_err
  in
  let input_fn fn =
    let chn_in = open_in fn in
    let buff = Buffer.create 13 in
    Buffer.add_channel buff chn_in (in_channel_length chn_in);
    close_in chn_in;
    Buffer.contents buff
  in
  try
    let stdin_in, stdin_out = Unix.pipe () in
    let command_array = Array.of_list (prog :: cli) in
    let command = String.concat " " (Array.to_list command_array) in
    let pid =
      Unix.create_process_env prog command_array env stdin_in
        (Unix.descr_of_out_channel chn_out)
        (Unix.descr_of_out_channel chn_err)
    in
    let () =
      Unix.close stdin_in;
      Unix.close stdin_out
    in
    let return_code =
      match snd (Unix.waitpid [] pid) with
      | Unix.WEXITED code | Unix.WSIGNALED code | Unix.WSTOPPED code -> code
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
    (command, return_code, out, err)
  with e ->
    cleanup ();
    raise e

type tests = {
  verbose : bool;
  search_path : string list;
  ocaml_xgettext : string;
  ocaml_gettext : string;
  test_dir : string;
  install_dir : string;
}

let parse_arg () =
  let tests =
    ref
      {
        verbose = false;
        search_path = [];
        ocaml_xgettext =
          make_filename [ parent_dir; "_build"; "bin"; "ocaml-xgettext" ];
        ocaml_gettext =
          make_filename [ parent_dir; "_build"; "bin"; "ocaml-gettext" ];
        test_dir = make_filename [ current_dir; "testdata" ];
        install_dir = make_filename [ current_dir; "testinstall" ];
      }
  in
  Arg.parse
    (Arg.align
       [
         ( "--search",
           Arg.String
             (fun dir ->
               tests := { !tests with search_path = dir :: !tests.search_path }),
           "dir Search the specified directory for MO file." );
         ( "--verbose",
           Arg.Unit (fun () -> tests := { !tests with verbose = true }),
           "Processs with a lot of message." );
         ( "--ocaml-xgettext",
           Arg.String (fun s -> tests := { !tests with ocaml_xgettext = s }),
           "cmd Specify the ocaml-xgettext executable." );
         ( "--ocaml-gettext",
           Arg.String (fun s -> tests := { !tests with ocaml_gettext = s }),
           "cmd Specify the ocaml-gettext executable." );
         ( "--test-dir",
           Arg.String (fun s -> tests := { !tests with test_dir = s }),
           "dir Specify the temporary dir for testing files." );
       ])
    (fun _str -> ())
    ( "Test utility for ocaml-gettext v" ^ GettextConfig.version
    ^ " by Sylvain Le Gall\n"
    ^ "Copyright (C) 2004-2008 Sylvain Le Gall <sylvain@le-gall.net>\n"
    ^ "Licensed under LGPL v2.1 with OCaml exception." );
  !tests

(**********************************)
(* Test of Gettext implementation *)
(**********************************)

let implementation_test tests realize_data =
  (* Generate a test case of simple load of a MO file using an implementation *)
  let test_load parameters_lst (realize_str, realize) =
    let test_load_one realize parameters =
      (* Extract usefull information out of parameters *)
      let fl_mo = parameters.fl_mo in
      let test_translations = parameters.translations in
      (* Build t *)
      let t = t_of_parameters parameters in
      (* Build t' *)
      let t' = realize t in
      let test_one_translation translation =
        (* We cannot compare directly extracted values and t' extracted
           value , since we have a charset translation *)
        try
          match translation with
          | Singular (str_id, _) -> ignore (GettextCompat.gettext t' str_id)
          | Plural (str_id, str_plural, _) ->
              (* Using values from 0 to 2, we cover most of the plural cases *)
              ignore (GettextCompat.ngettext t' str_id str_plural 0);
              ignore (GettextCompat.ngettext t' str_id str_plural 1);
              ignore (GettextCompat.ngettext t' str_id str_plural 2)
        with exc ->
          assert_failure
            ( Printexc.to_string exc ^ " in "
            ^ string_of_translation translation )
      in
      fl_mo >:: fun () -> List.iter test_one_translation test_translations
    in
    realize_str >::: List.map (test_load_one realize) parameters_lst
  in
  (* Generate a cross test of string extracted, using different implementation *)
  let test_cross implementation_lst parameters =
    (* Extract usefull information out of parameters *)
    let fl_mo = parameters.fl_mo in
    let test_translations = parameters.translations in
    (* Build t *)
    let t = t_of_parameters parameters in
    (* Build all t' *)
    let t'_lst =
      List.map
        (fun (realize_str, realize) -> (realize_str, realize t))
        implementation_lst
    in
    let check_translation str lst =
      let _, same_str =
        List.fold_left
          (fun (prev_str_opt, res) (_, cur_str) ->
            match prev_str_opt with
            | Some prev_str -> (Some cur_str, res && prev_str = cur_str)
            | None -> (Some cur_str, res))
          (None, true) lst
      in
      if same_str then ()
      else
        assert_failure
          (Printf.sprintf
             "All values should be identical in [ %s ] in function %s"
             (String.concat " ; "
                (List.map
                   (fun (realize_str, str) ->
                     Printf.sprintf "(%s,%S)" realize_str str)
                   lst))
             str)
    in
    let test_cross_one translation =
      match translation with
      | Singular (str_id, _) ->
          check_translation
            (Printf.sprintf "GettextCompat.gettext t' %S" str_id)
            (List.map
               (fun (realize_str, t') ->
                 (realize_str, GettextCompat.gettext t' str_id))
               t'_lst)
      | Plural (str_id, str_plural, _) ->
          List.iter
            (fun n ->
              check_translation
                (Printf.sprintf "GettextCompat.ngettext t' %S %S %d" str_id
                   str_plural n)
                (List.map
                   (fun (realize_str, t') ->
                     ( realize_str,
                       GettextCompat.ngettext t' str_id str_plural n ))
                   t'_lst))
            [ 0; 1; 2 ]
    in
    fl_mo >:: fun () -> List.iter test_cross_one test_translations
  in
  (* Extract and test *)
  let parameters_lst =
    List.map parameters_of_filename (mo_files_data tests.test_dir)
  in
  let implementation_lst = realize_data in
  "Gettext implementation test"
  >::: [
         "Load" >::: List.map (test_load parameters_lst) implementation_lst;
         "Cross check"
         >::: List.map (test_cross implementation_lst) parameters_lst;
       ]
