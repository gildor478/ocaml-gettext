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

(** Function for manipulation the type t *)

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

let create 
  ?(failsafe = Ignore) 
  ?(categories = []) 
  ?(codesets = [])
  ?(dirs = []) 
  ?(textdomains = [])
  ?(codeset)
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
        failsafe    = failsafe;
        textdomains = MapTextdomain.empty;
        categories  = map_categories;
        language    = language;
        codeset     = codeset;
        default     = textdomain;
      }
    in
    (* Apply any upgrade required by the different settings provided *)
    let apply_upgrade t lst =
      List.fold_left (
        fun t (textdomain,changes) ->
          upgrade_textdomain t textdomain changes
      ) t lst
    in
    (* All changes from the setting of textdomains *)
    let textdomains_changes = 
      List.map 
      (fun textdomain -> (textdomain,(None,None))) 
      (textdomain :: textdomains)
    in
    (* All changes from the setting of codesets *)
    let codesets_changes =
      List.map 
      (fun (textdomain,codeset) -> (textdomain,(Some codeset,None))) 
      codesets
    in
    (* All changes from the setting of dirs *)
    let dirs_changes =
      List.map 
      (fun (textdomain,dir) -> (textdomain,(None,Some dir))) 
      dirs
    in
    apply_upgrade 
    result 
    ( textdomains_changes @ codesets_changes @ dirs_changes )
;;
