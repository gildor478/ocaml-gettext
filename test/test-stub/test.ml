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

open OUnit
open FileUtil
open Common

(* Different implementation of realize. *)
let realize_data =
  [
    ("Stub.Native", GettextStub.Native.realize);
    ("Stub.Preload", GettextStub.Preload.realize);
  ]

(************************************************)
(* Set bad locale and spot error when setlocale *)
(* returns NULL                                 *)
(************************************************)

let bad_setlocale =
  "Call setlocale with bad locale"
  >::: [
         ( "setlocale with bad locale" >:: fun () ->
           ignore (GettextStubCompat.setlocale GettextStubCompat.LC_ALL "xx")
         );
       ]

(******************)
(* Try to compile *)
(******************)

let compile_ocaml tests =
  "Compile OCaml code"
  >::: List.map
         (fun (bn, exp_return_code, exp_err) ->
           bn >:: fun () ->
           let command, return_code, _out, err =
             run_and_read "ocamlc"
               [
                 "-c";
                 "-I";
                 "../../src/lib/gettext/base/.gettextBase.objs/byte";
                 "-I";
                 "../../src/lib/gettext-stub/.gettextStub.objs/byte";
                 "-I";
                 tests.test_dir;
                 Filename.concat tests.test_dir "TestGettext.ml";
                 Filename.concat tests.test_dir bn;
               ]
           in
           FileUtil.rm
             (List.map (FilePath.replace_extension bn) [ "cmo"; "cmi" ]);
           FileUtil.rm
             (List.map (FilePath.add_extension "TestGettext") [ "cmo"; "cmi" ]);
           assert_bool
             (Printf.sprintf
                "error output of %S:\nwant to contain: %S\ngot:\n%s"
                command exp_err err)
             (BatString.exists err exp_err);
           assert_equal
             ~msg:("return code of " ^ command)
             ~printer:string_of_int exp_return_code return_code)
         [
           ("unsound_warning.ml", 0, "");
           ("valid_format.ml", 0, "");
           ("invalid_format1.ml", 2, "line 4, characters 28-29:");
           ("invalid_format2.ml", 2, "line 4, characters 28-29:");
           ("invalid_format3.ml", 2, "line 4, characters 27-28:");
           ("invalid_format4.ml", 2, "line 4, characters 27-28:");
           ("invalid_format5.ml", 2, "line 4, characters 36-52:");
         ]

let () =
  let tests = parse_arg () in
  let all_test =
    "Test ocaml-gettext"
    >::: [
      bad_setlocale;
      compile_ocaml tests;
      implementation_test tests realize_data;
    ]
  in
  mkdir ~parent:true tests.test_dir;
  ignore(run_test_tt all_test)
