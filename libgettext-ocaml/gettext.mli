(** Interface of all module implementing gettext functions *)

(** init default_lang textdomain categories : Initialize the library globally.
    default_lang should be a well formed ISO code for language. If you use None
    for default_lang, a value should be guessed using environnement, falling
    back to C default if guess failed. textdomain is the default catalog used 
    to lookup string. You can define more category binding usnig categories.
*)
val init : 
     string option
  -> string 
  -> ?(categories : (GettextTypes.locale_category_type * string) list) 
  -> unit 

(** gettext str : Translate the string str 
*)
val gettext : string -> string

(** dgettext domain str : Translate the string str for the specified domain 
*)
val dgettext : string -> string -> string

(** dcgettext domain str category : Translate the string str for the specified
    domain and category.
*)
val dcgettext : string -> string -> GettextTypes.locale_category_type -> string

(** ngettext str str_plural n : Translate the string str using a plural form.
    str_plural is the default english plural. n is the relevant number for plural
    ( ie the number of objects deals with the string ).
*)
val ngettext : string -> string -> int -> string

(** dngettext domain str str_plural n : Translate the string str using a plural
    form for the specified domain.
*)
val dngettext : string -> string -> string -> int -> string

(** textdomain domain : set the current text domain 
*)
val textdomain : string -> unit

(** get_textdomain () : returns the current text domain 
*)
val get_textdomain : unit -> string

(** bindtextdomain domain dir : set the default base directory for the specified
    domain.
*)
val bindtextdomain : string -> string -> unit

(** bind_textdomain_codeset domain codeset : set the codeset to use for the
    specified domain. codeset must be a valid codeset for the underlying
    character encoder/decoder ( iconv, camomile, extlib... )
*)
val bind_textdomain_codeset : string -> string -> unit
