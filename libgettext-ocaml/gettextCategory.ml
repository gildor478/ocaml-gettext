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

let compare c1 c2 = 
  let val_category x = 
    match x with 
      LC_CTYPE    -> 0 
    | LC_NUMERIC  -> 1
    | LC_TIME     -> 2
    | LC_COLLATE  -> 3
    | LC_MONETARY -> 4
    | LC_MESSAGES -> 5
    | LC_ALL      -> 6
  in
  compare (val_category c1) (val_category c2)
;;
