open GettextTypes;;
open GettextCompat;;

(* Function the main global variable of gettext with/without thread *)

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

let get_global_t' () =

(* High level functions *)

module Library =
  functor ( Init : Init ) ->
  struct
    let init = (Init.textdomain, Init.codeset, Init.dir) :: Init.dependencies

    let s_   = dgettext (get_global_t' ()) Init.textdomain 
    let f_   = fdgettext (get_global_t' ()) Init.textdomain
    let sn_  = dngettext (get_global_t' ()) Init.textdomain
    let fn_  = fdngettext (get_global_t' ()) Init.textdomain
  end
;;

module Program = 
  functor ( Init : Init ) ->
  functor ( Realize : realize ) ->
  struct
    let textdomain = 
      let (textdomain,_,_) = Init
      in
      textdomain

    let init = [  
      (
        "--gettext-failsafe",
        ( Arg.Symbol 
          (
            ["ignore"; "inform-stderr"; "raise-exception"],
            ( fun x ->
                match x with
                  "ignore"          -> set_global { (get_global ()) with failsafe = Ignore }
                | "inform-stderr"   -> set_global { (get_global ()) with failsafe = InformStderr }
                | "raise-exception" -> set_global { (get_global ()) with failsafe = RaiseException }
            )
          )
        ),
        "Choose how to handle failure in gettext ( ignore, stderr, exception )" 
      );
      (
        "--gettext-disable",
        ( Arg.Unit 
          ( fun () -> set_global { (get_global ()) with realize = XXX } 
          )
        ),
        "Disable the translation perform by gettext"
      );
      (
        "--gettext-domain-dir",
        ( Arg.Tuple 
          (
            Arg.String , Arg.String 
          )
        )
        "Set a dir to search gettext files for the specified domain"
      );
      (
        "--gettext-dir",
        ( Arg.String
          ( fun s -> set_global { (get_global ()) with dir = s }
          )
        ),
        "Set the default dir to search gettext files"
      );
      (
        "--gettext-language",
        ( Arg.String
          ( fun s -> set_global { (get_global ()) with language = s }
          )
        ),
        "Set the default language for gettext"
      );
      (
        "--gettext-codeset",
        ( Arg.String
          ( fun s -> set_global { (get_global ()) with codeset = s }
          )
        ),
        "Set the default codeset for outputting string with gettext"
      );
      ]
      
    let s_   = dgettext (get_global_t' ()) textdomain 
    let f_   = fdgettext (get_global_t' ()) textdomain
    let sn_  = dngettext (get_global_t' ()) textdomain
    let fn_  = fdngettext (get_global_t' ()) textdomain

  end
;;
