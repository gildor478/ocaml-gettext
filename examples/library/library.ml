
open LibraryGettext;;

(* Give access to the init of LibraryGettext *)
let init =
  Gettext.init
;;

(* Example functions *)
let library_only_function () = 
  
  (* Two simple examples : singular translation *)
  print_endline (s_ "Hello world !");
  Printf.printf (f_ "Hello %s !") "world";
  
  (* More complicated : plural translation, using strings *)
  print_endline (
     (sn_ "There is " "There are " 2)
    ^(string_of_int 2)
    ^(sn_ "plate." "plates." 2)
  );
  
  (* More simple forms of plural translation, using printf *)
  Printf.printf (fn_ "There is %d plates." "There are %d plates" 2) 2
;;
  
