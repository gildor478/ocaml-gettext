(** Implements different operation over locale/category *)

open GettextTypes;;

module type LOCALE_TYPE = 
  sig
    type locale   = string
    type encoding = string
    type category
    type t 
    
    (** Create the locale structure.
    *)
    val create : failsafe -> t
    
    (** compare_category c1 c2 : Compare category c1 and c2.
    *)
    val compare_category : category -> category -> int

    (** string_of_catagory c : Return the string corresponding to c.
    *)
    val string_of_category : category -> string

    (** set_locale cat loc t : Set the category cat to be of value loc for
        the locale structure t. loc should be written using ASCII. This
        functions could raise any exception depending on implementation.
    *)
    val set_locale : category -> locale -> t -> t 
    
    (** get_locale cat t : Return the value of category cat for locale 
        structure t. The string returned is in ASCII.
    *)
    val get_locale : category -> t -> locale

    (** default_charset t : Return the default charset for the locale 
        structure t.
    *)
    val default_charset : t -> encoding

    (** messages : Value representing LC_MESSAGES ( default category of 
        catalog for gettext ).
    *)
    val messages : category

    (** all : Value representing LC_ALL ( super category of all categories ).
    *)
    val all : category
  end
;;
