
open GettextUtils;;

module MapTextdomain = MapString;;
module MapCategory = Map.Make();;

type t = {
  failsafe    = failsafe;
  textdomains = ((codeset option) * (dir option)) MapTextdomain.t;
  categories  = locale MapCategory.t;
  language    = locale option;
  codeset     = codeset option;
  default     = textdomain;
}
;;
  
type t' = textdomain option -> string -> (string * int) option -> category -> string
;;

(* Functions for manipulation the type t : create, textdomain, get_textdomain, 
*  bindtextdomain, bindtextdomain_codeset *)

(* Utility functions *)
let upgrade_textdomain t k value =  
  let (current_codeset,current_dir) = 
    try
      MapTextdomain.find k t.textdomains
    with Not_found ->
      (None,None)
  in
  let new_value = 
    match value with
      (None,None) -> (current_codeset,current_dir)
    | (None,new_dir) -> (current_codeset,new_dir)
    | (new_codeset,None) -> (new_codeset,current_dir)
    | (new_codeset,new_dir) -> (new_codeset,new_dir)
  in
  { t with textdomains = MapTextdomain.add k new_value t.textdomains }
;;

let bindtextdomain textdomain dir t =
  upgrade_textdomain t textdomain (None,Some dir)
;;
  
let bindtextdomain_codeset textdomain codeset t =
  upgrade_textdomain t textdomain (Some codeset,None)
;;

let textdomain default t = 
  { t with default = default }
;;

let get_textdomain t = 
  t.default
;;

let create 
  ?(failsafe = Ignore) 
  ?(categories = []) 
  ?(codesets = [])
  ?(dirs = []) 
  ?(textdomains = [])
  ?(language)
  textdomain =
    let map_categories = 
        List.fold_left ( 
          fun map (category,locale) -> 
            MapCategory.add category locale map 
          ) 
        categories
    in
    let result =
      {
        failsafe = failsafe;
        textdomains = MapTextdomain.empty;
        categories = map_categories; 
        language = language;
        default = textdomain;
      }
    in
    let apply_upgrade t (to_std,lst) =
      List.fold_left (
        fun t (textdomain,changes) ->
          upgrade_textdomain t textdomain changes
      ) result (List.map to_std lst)
    in
    List.fold_left apply_upgrade result [
      (fun textdomain -> (textdomain,(None,None))), (textdomain :: textdomains);
      (fun (textdomain,codeset) -> (textdomain,(Some codeset,None))), codeset;
      (fun (textdomain,dir) -> (textdomain,(None,Some dir))) dirs
    ]
;;

(* Functions for doing real translation *)

let format_of_string = 
  Obj.magic
;;

let gettext t' str =
  t' None str None Locale.messages
;;

let fgettext t' str =
  format_of_string gettext
;;
  
let dgettext t' textdomain str =
  t' (Some domain) str None Locale.messages
;;

let fdgettext t' domain str =
  format_of_string dgettext
;;
  
let dcgettext t' textdomain str category = 
  t' (Some domain) str None category
;;

let fdcgettext t' domain str category =
  format_of_string dcgettext
;;
  
let ngettext t' str str_plural n =
  t' None str (Some(str_plural,n)) Locale.messages
;;

let fngettext t' str str_plural n =
  format_of_string ngettext
;;
  
let dngettext t' domain str str_plural n =
  t' (Some domain) str (Some (str_plural,n)) Locale.messages
;;

let fdngettext t' domain str str_plural n =
  format_of_string dngettext
;;

let dcngettext t' domain str str_plural n category = 
  t' (Some domain) str (Some(str_plural,n)) category
;;
  
let fdcngettext =
  format_of_string dcngettext
;;

(* High level functions *)

type global_type = {
  failsafe   : failsafe;
  categories : category list;
  codesets   : (textdomain * codeset) list;
  dirs       : (textdomain * dir) list;
  codeset    : codeset
  language   : language;
}
;;

let global = ref []
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

let get_

module Library =
  functor ( Init : init ) ->
  functor ( InitDependencies : init list ) ->
  struct
    let textdomain = 
      let (textdomain,_,_) = Init 
      in
      textdomain

    let init = Init :: InitDependencies

    let s_   = dgettext (get_global_t' ()) textdomain 
    val f_   = fdgettext (get_global_t' ()) textdomain
    val sn_  = dngettext (get_global_t' ()) textdomain
    val fn_  = fdngettext (get_global_t' ()) textdomain
  end
;;

module Program = 
  functor ( Init : init ) ->
  functor ( InitDependencies : init list ) ->
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
            ( fun 
                  "ignore"          -> set_global { get_global () with failsafe = Ignore }
                | "inform-stderr"   -> set_global { get_global () with failsafe = InformStderr }
                | "raise-exception" -> set_global { get_global () with failsafe = RaiseException }
            )
          )
        ),
        "Choose how to handle failure in gettext ( ignore, stderr, exception )" 
      );
      (
        "--gettext-disable",
        ( Arg.Unit 
          ( fun () -> set_global { get_global () with realize = XXX } 
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
          ( fun s -> set_global { get_global () with dir = s }
          )
        ),
        "Set the default dir to search gettext files"
      );
      (
        "--gettext-language",
        ( Arg.String
          ( fun s -> set_global { get_global () with language = s }
          )
        ),
        "Set the default language for gettext"
      );
      (
        "--gettext-codeset",
        ( Arg.String
          ( fun s -> set_global { get_global () with codeset = s }
          )
        ),
        "Set the default codeset for outputting string with gettext"
      );
      ]
      
    let s_   = dgettext (get_global_t' ()) textdomain 
    val f_   = fdgettext (get_global_t' ()) textdomain
    val sn_  = dngettext (get_global_t' ()) textdomain
    val fn_  = fdngettext (get_global_t' ()) textdomain

  end
;;
