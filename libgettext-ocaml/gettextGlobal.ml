open GettextTypes;;

(** Function the main global variable of gettext with/without thread *)

type global_type = {
  failsafe   : failsafe;
  categories : category list;
  codesets   : (textdomain * codeset) list;
  dirs       : (textdomain * dir) list;
  codeset    : codeset
  language   : language;
}
;;

let critical_section = ref ( fun x -> x () )
;;

let global = ref {
  failsafe   = Ignore;
  categories = [];
  codesets   = [];
  dirs       = [];
  codeset    = codeset;
  language   = language;
}
;;

let get_global () = 
  !critical_section ( 
    fun () ->
      !global
  )
;;

let set_global glb = 
  !critical_section (
    fun () ->
      global := glb
  )
;;
