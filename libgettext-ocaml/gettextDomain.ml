(** Signature of module for domain management *)

module type DOMAIN_TYPE = 
  sig
    type t
    type language_code = string
    type textdomain = string
    type dir = string
    type category = GettextTypes.locale_category_type
    
    val create : ?language:string -> unit -> t

    val add_category : category -> language_code -> t -> t

    val add_textdomain : textdomain -> dir -> t -> t 

    val compute_path : textdomain -> category -> t -> dir
    
  end
;;

