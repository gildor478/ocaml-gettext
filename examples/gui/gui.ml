
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
