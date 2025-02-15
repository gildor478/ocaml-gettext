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

open Benchmark
open Common
open GettextTypes

type benchs = { verbose : bool; test_dir : string; time : int }

(* Different implementation of realize. *)
let realize_data =
  [
    ("Camomile.Map", GettextCamomile.Map.realize);
    ("Camomile.Hashtbl", GettextCamomile.Hashtbl.realize);
    ("Camomile.Open", GettextCamomile.Open.realize);
    ("Stub.Native", GettextStub.Native.realize);
    ("Stub.Preload", GettextStub.Preload.realize);
  ]

let parse_arg () =
  let benchs = ref { verbose = false; test_dir = Sys.getcwd (); time = 1 } in
  Arg.parse
    (Arg.align
       [
         ( "--test_dir",
           Arg.String (fun dir -> benchs := { !benchs with test_dir = dir }),
           "dir Search the specified directory for MO file." );
         ( "--verbose",
           Arg.Unit (fun () -> benchs := { !benchs with verbose = true }),
           "Processs with a lot of message." );
         ( "--time",
           Arg.Int (fun sec -> benchs := { !benchs with time = sec }),
           Printf.sprintf
             "second Process each test during the specified number of second. \
              Default : %d."
             !benchs.time );
       ])
    (fun _str -> ())
    ("Benchmark utility for ocaml-gettext v" ^ GettextConfig.version
   ^ " by Sylvain Le Gall\n"
   ^ "Copyright (C) 2004-2008 Sylvain Le Gall <sylvain@le-gall.net>\n"
   ^ "Licensed under LGPL v2.1 with Ocaml exception.");
  !benchs

let print_debug benchs str =
  if benchs.verbose then (
    print_string str;
    print_newline ())
  else ()

let make_buffer lst = (lst, [])

let get_buffer (lst1, lst2) =
  match (lst1, lst2) with
  | hd :: tl, lst2 -> (hd, (tl, hd :: lst2))
  | [], hd :: tl -> (hd, (tl, [ hd ]))
  | [], [] -> failwith "Buffer is empty"

type fixture = { textdomain : string; realized_lst : (string * t') list }

(* Set up benchmarks. *)
let set_up benchs =
  let parameters =
    parameters_of_filename benchs.test_dir
      (FilePath.DefaultPath.make_filename
         [ "fr_FR"; "LC_MESSAGES"; "test2.mo" ])
  in
  let t = t_of_parameters parameters in
  let realized_lst =
    List.map (fun (name, realize) -> (name, realize t)) realize_data
  in
  { textdomain = parameters.textdomain; realized_lst }

(*******************************)
(* Performance of check_format *)
(*******************************)

let format_bench benchs =
  let f ref_buffer =
    let elem, buffer = get_buffer !ref_buffer in
    let translation =
      print_debug benchs ("Checking format of : " ^ string_of_translation elem);
      GettextFormat.check_format Ignore elem
    in
    print_debug benchs
      ("Result of the check : " ^ string_of_translation translation);
    ref_buffer := buffer
  in
  print_debug benchs "Benchmarking format :";
  ( "Format benchmark",
    throughputN benchs.time
      [
        ("Singular", f, ref (make_buffer format_translation_singular_data));
        ("Plural", f, ref (make_buffer format_translation_plural_data));
        ("All", f, ref (make_buffer format_translation_all_data));
      ] )

(***************************)
(* Performance of realize  *)
(***************************)

let realize_bench benchs =
  let f (realize, parameters_lst) =
    let f_one parameters =
      let t =
        print_debug benchs ("Creating t for " ^ parameters.fl_mo);
        t_of_parameters parameters
      in
      print_debug benchs ("Realizing t for " ^ parameters.fl_mo);
      ignore (realize t)
    in
    List.iter f_one parameters_lst
  in
  let parameters_lst =
    List.map (parameters_of_filename benchs.test_dir) mo_files_data
  in
  let bench_lst =
    List.map
      (fun (str_implementation, realize) ->
        (str_implementation, f, (realize, parameters_lst)))
      realize_data
  in
  print_debug benchs "Benchmarking realize:";
  ("Realize benchmark", throughputN benchs.time bench_lst)

(**********************)
(* Performance of s_  *)
(**********************)

let s_bench benchs =
  let fun_gettext t' textdomain =
    let _ : string = GettextCompat.dgettext t' textdomain "" in
    let _ : string =
      GettextCompat.dgettext t' textdomain "%s is replaced by %s."
    in
    let _ : string = GettextCompat.dgettext t' textdomain "hey" in
    ()
  in
  let fixture = set_up benchs in
  ( "s_ benchmark",
    throughputN benchs.time
      (List.map
         (fun (name, t') -> (name, fun_gettext t', fixture.textdomain))
         fixture.realized_lst) )

(*********************)
(* Performance of f_ *)
(*********************)

let f_bench benchs =
  let fun_gettext t' textdomain =
    let _ : string =
      Printf.sprintf
        (GettextCompat.fdgettext t' textdomain
           "'Your command, please?', asked the waiter.")
    in
    (* TODO: gettextFormat doesn't handle well %2$s in the translation. *)
    (*     let _ : string = Printf.sprintf (GettextCompat.fdgettext t' textdomain "%s is replaced by %s.") "a" "b" in *)
    let _ : string =
      Printf.sprintf (GettextCompat.fdgettext t' textdomain "hey")
    in
    ()
  in
  let fixture = set_up benchs in
  ( "f_ benchmark",
    throughputN benchs.time
      (List.map
         (fun (name, t') -> (name, fun_gettext t', fixture.textdomain))
         fixture.realized_lst) )

(**********************)
(* Performance of sn_ *)
(**********************)

let sn_bench benchs =
  let fun_gettext t' textdomain =
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 0
    in
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 1
    in
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 2
    in
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 3
    in
    ()
  in
  let fixture = set_up benchs in
  ( "sn_ benchmark",
    throughputN benchs.time
      (List.map
         (fun (name, t') -> (name, fun_gettext t', fixture.textdomain))
         fixture.realized_lst) )

(**********************)
(* Performance of fn_ *)
(**********************)

let fn_bench benchs =
  let fun_gettext t' textdomain =
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 0
    in
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 1
    in
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 2
    in
    let _ : string =
      GettextCompat.dngettext t' textdomain "%d coffee" "more %d coffee" 3
    in
    ()
  in
  let fixture = set_up benchs in
  ( "fn_ benchmark",
    throughputN benchs.time
      (List.map
         (fun (name, t') -> (name, fun_gettext t', fixture.textdomain))
         fixture.realized_lst) )

(**************************)
(* Main benchmark routine *)
(**************************)
;;

let benchs = parse_arg () in
let () = print_endline benchs.test_dir in
let all_bench =
  [ format_bench; realize_bench; s_bench; f_bench; sn_bench; fn_bench ]
in

(* Running *)
let all_results = List.map (fun x -> x benchs) all_bench in
List.iter
  (fun (str, results) ->
    print_newline ();
    print_newline ();
    print_endline str;
    tabulate results)
  all_results
