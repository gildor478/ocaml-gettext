open GettextGlobal;;

(** Register a special critical section handling *)

let gettext_mutex = Thread.create ()
;;

critical_section := fun x ->
  let res = 
    Thread.lock gettext_mutex;
    x ()
  in
  Thread.unlock gettext_mutex;
  res
;;
  
