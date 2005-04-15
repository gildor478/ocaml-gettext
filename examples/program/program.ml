(**************************************************************************)
(*  Ocaml-gettext : example code                                           *)
(*                                                                         *)
(*  Copyright (C) 2003, 2004, 2005 Sylvain Le Gall <sylvain@le-gall.net>   *)
(*                                                                         *)
(*  You can redistribute this library and/or modify it under the terms of  *)
(*  the GNU LGPL v2.1 with the OCaml static compilation exception.         *)
(*                                                                         *)
(*  Contact: sylvain@le-gall.net                                           *)
(**************************************************************************)

open ProgramGettext.Gettext;;

let () = 
  let my_name = ref ""
  in
  let spf x = Printf.sprintf x
  in
  let (gettext_args,gettext_copyright) =
    ProgramGettext.Gettext.init
  in
  let args =
    Arg.align (
      [
        "--my-name",
        Arg.String ( fun s ->
          my_name := s
        ),
        ( spf (f_ "name Your name. Default : %S") !my_name )
      ] @ gettext_args
    )
  in
  let () =
    Arg.parse
    args
    ( fun str -> () )
    (
      spf (f_ 
"\"Hello you\" program by Sylvain Le Gall

%s

Command: program [options]

Options:") gettext_copyright
    )
  in
  Library.hello_you !my_name;
  Gui.hello_you !my_name
;;
