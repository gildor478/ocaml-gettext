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

open GuiGettext;;

(* Give access to the init of GuiGettext *)
let init = 
  Gettext.init
;;

(* Build a simple window that display your name *)
let hello_you name = 
  let spf x = Printf.sprintf x
  in
  let window = GWindow.window ~title:(s_ "Hello world !") ~border_width:12 () 
  in
  let label = GMisc.label ~text:(spf (f_ "Hello %s") name) ~packing:window#add ()
  in
  ignore (window#event#connect#delete ~callback:(fun _ -> false));
  ignore (window#connect#destroy ~callback:(fun _ -> GMain.Main.quit ()));
  window#show ();
  GMain.Main.main ()
;;
