(** Implements different operation over locale/category *)

module type LOCALE_TYPE = 
  sig
    type locale   = string
    type category
    type t 
    
    val create : unit -> t
    
    val compare : category -> category -> int

    val set_locale : category -> locale -> t -> t 
    
    val get_locale : category -> t -> locale
  end
;;

let compare a b = 
  let value_of_category c =
    match c with
      LC_CTYPE    -> 0
    | LC_NUMERIC  -> 1
    | LC_TIME     -> 2
    | LC_COLLATE  -> 3
    | LC_MONETARY -> 4
    | LC_MESSAGES -> 5
    | LC_ALL      -> 6
  in
  (value_of_category a) - (value_of_category b)
;;

let string_of_category c = 
   match c with
    LC_CTYPE    -> "LC_CTYPE" 
  | LC_NUMERIC  -> "LC_NUMERIC"
  | LC_TIME     -> "LC_TIME"
  | LC_COLLATE  -> "LC_COLLATE"
  | LC_MONETARY -> "LC_MONETARY"
  | LC_MESSAGES -> "LC_MESSAGES"
  | LC_ALL      -> "LC_ALL"
;;
