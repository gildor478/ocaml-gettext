open FileUtil;;
open FileUtil.StrUtil;;
open GettextMo;;
open GettextTypes;;

(** Exceptions *)

exception GettextUninitialized;;
exception GettextMoFileNotFound;;
exception GettextNoTranslation of string;;

let string_of_exception exc =
  match exc with
    GettextUninitialized -> 
      "Gettext is not initialized"
  | GettextMoFileNotFound -> 
      "Gettext could not find the corresponding mo file"
  | GettextNoTranslation str_id -> 
      "Gettext could not find translation for string id \""^str_id^"\""
  | _ ->
      ""
;;

(** Functions for manipulation the type t : create, textdomain, get_textdomain,
    bindtextdomain, bindtextdomain_codeset *)

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
        MapCategory.empty
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
