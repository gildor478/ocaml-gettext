open Benchmark;;

open Utils;;
open Data;;

open GettextTypes;;


type benchs = {
  verbose      : string;
  search_path  : string list;
  time         : int;
}
;;

let parse_arg () = 
  let benchs = ref {
    verbose     = false;
    search_path = [];
    time        = 10;
  }
  in
  Arg.parse (Arg.align [
    (
      "--search", Arg.String (
        fun dir ->
          benchs := { !benchs with search_path = dir :: !benchs.search_path }
        )
      ,"dir Search the specified directory for MO file."
    )
    (
      "--verbose", Arg.Unit (
        fun () ->
          benchs := { !benchs with verbose = true }
        )
      ,"Processs with a lot of message."
    );
    (
      "--time", Arg.Int (
        fun sec ->
          benchs := { !benchs with time = sec }
        )
      ,(Print.sprintf "second Process each test during the specified number of second. Default : %d." 
      !benchs.time)
    )
  ])
  ( fun str -> () )
  ("Benchmark utility for ocaml-gettext v"^(GettextConfig.version)^" by Sylvain Le Gall\n"^
  "Copyright 2004,2005. Licensed under LGPL v2.1 with Ocaml exception.");
  !benchs
;;

let print_debug benchs str =
  if benchs.verbose then
    (print_string str; print_newline ())
  else
    ()
;;

let rec get_buffer (lst1,lst2) =
  match (lst1,lst2) with
   (hd :: tl,lst2) ->
    (hd,(tl, hd :: lst2))
  | ([], hd :: tl) ->
    (hd, (tl, [hd]))
  | ([],[]) ->
      raise Empty
;;
      
  

(*******************************)
(* Performance of check_format *)
(*******************************)

let format_bench benchs =
  let f ref_buffer = 
    let (elem,buffer) =
      get_buffer !ref_buffer
    in
    let translation =
      GettextFormat.check_format elem
    in
    ref_buffer := buffer
  in  
  print_string "Benchmarking format :";
  print_newline ();
  throughputN benchs.time [
    ("Only singular", f, ref format_translation_singular_data);
    ("Only plural"  , f, ref format_translation_plural_data);
    ("All"          , f, ref format_translation_all_data);
  ]
;;

(***************************)
(* Performance of realize  *)
(***************************)

(**********************)
(* Performance of s_  *)
(**********************)

(*********************)
(* Performance of f_ *)
(*********************)

(**********************)
(* Performance of sn_ *)
(**********************)

(**********************)
(* Performance of fn_ *)
(**********************)
