(** Functions to manipulate category *) 

open GettextTypes;;

let string_of_category cat =
  match cat with
    LC_CTYPE    -> "LC_CTYPE"
  | LC_NUMERIC  -> "LC_NUMERIC"
  | LC_TIME     -> "LC_TIME"
  | LC_COLLATE  -> "LC_COLLATE"
  | LC_MONETARY -> "LC_MONETARY"
  | LC_MESSAGES -> "LC_MESSAGES"
  | LC_ALL      -> "LC_ALL"
;;
 
let categories = [
  LC_CTYPE ;
  LC_NUMERIC ;
  LC_TIME ;
  LC_COLLATE ;
  LC_MONETARY ;
  LC_MESSAGES ;
  LC_ALL 
]
;;
