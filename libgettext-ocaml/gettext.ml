
open GettextUtils;;

module MapTextdomain = MapString;;
module MapCategory = Map.Make();;

type t = {
  failsafe    = failsafe;
  textdomains = ((codeset option) * (dir option)) MapTextdomain.t;
  categories  = locale MapCategory.t;
  language    = locale;
  default     = textdomain;
}
;;
  
type t' = textdomain -> string -> string -> int -> category -> string
;;

(* Functions for manipulation the type t : create, textdomain, add_textdomain,
* get_textdomain, bindtextdomain, bindtextdomain_codeset *)

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

let add_textdomain textdomain t =
  upgrade_textdomain t textdomain (None,None)
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
    let result = 
      List.fold_left ( 
        fun t textdomain -> 
          add_textdomain textdomain t 
      )
      result
      (textdomain :: textdomains)
    in
    let result = 
      List.fold_left ( 
        fun t (textdomain,codeset) ->
          bindtextdomain_codeset textdomain codeset t
      )
      result
      codesets
    in
    let result =
      List.fold_left ( 
        fun t (textdomain,dir) ->
          bindtextdomain textdomain dir t
      )
      result
    in
    result
;;

(* Functions for doing real translation *)

let dcngettext t' domain str str_plural n category = 
  t' (Some domain) str (Some(str_plural,n)) category
;;
  
let fdcngettext t' domain str str_plural n category =
  Obj.magic (t' (Some domain) str str_plural n category)
;;

let gettext t' str =
  t' None str str 0 Locale.messages
;;

let fgettext t' str =
  fdcngettext t' None str str 0 Locale.messages
;;
  
let dgettext t' domain str =
  translate domain str Locale.messages
;;

let fdgettext t' domain str =
  format_of_string (dgettext domain str)
;;
  
let dcgettext t' domain str category = 
  translate domain str category
;;

let fdcgettext t' domain str category =
  format_of_string (dcgettext domain str category)
;;
  
let ngettext t' str str_plural n =
  translate (get_textdomain ()) str ~plural_form:(str_plural,n) Locale.messages
;;

let fngettext t' str str_plural n =
  format_of_string (ngettext str str_plural n)
;;
  
let dngettext t' domain str str_plural n =
  translate domain str ~plural_form:(str_plural,n) Locale.messages
;;

let fdngettext t' domain str str_plural n =
  format_of_string (dngettext domain str str_plural n)
;;

