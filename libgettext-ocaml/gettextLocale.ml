(** Implements different operation over locale/category *)

module type LOCALE_TYPE = 
  sig
    type locale   = string
    type encoding = string
    type category
    type t 
    
    val create : unit -> t
    
    val compare : category -> category -> int

    val set_locale : category -> locale -> t -> t 
    
    val get_locale : category -> t -> locale

    val default_charset : t -> encoding

    val messages : category
  end
;;
