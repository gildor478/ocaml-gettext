(** Signature of module for domain management *)

module type DOMAIN_TYPE = 
  sig
    type t
    type language_code = string
    type textdomain = string
    type dir = string
    type category = GettextTypes.locale_category_type
    
    val create : unit -> t

    val add_category : category -> language_code -> t -> t

    val remove_category : category -> t -> t

    val get_category : category -> t -> language_code

    val add_textdomain : textdomain -> dir -> t -> t 

    val remove_textdomain : textdomain -> t -> t

    val get_textdomain : textdomain -> t -> dir

    val compute_path : textdomain -> category -> t -> dir
    
  end
;;

