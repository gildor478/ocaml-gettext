(** Gettext functions *)

(** Exceptions *)

exception GettextUninitialized;;
exception GettextMoFileNotFound;;
exception GettextNoTranslation of string;;

val string_of_exception : exn -> string

(** Types *)

type textdomain = string
type locale     = string
type dir        = string
type codeset    = string
type category 

type t
type t'

(** Implementation of gettext functions *)

(** init failsafe categories codesets dirs language textdomain : Initialize the library globally.
    language should be a well formed ISO code one. If you don't set
    the language, a value should be guessed using environnement, falling
    back to C default if guess failed. textdomain is the default catalog used 
    to lookup string. You can define more category binding using categories.
    You can define codeset on a per domain basis.
*)
val init : 
     ?failsafe : failsafe  
  -> ?categories : (category * locale) list 
  -> ?codesets : (textdomain * codeset) list
  -> ?dirs : (textdomain * dir) list
  -> ?language : locale
  -> textdomain 
  -> t

(** textdomain domain : Set the current text domain.
*)
val textdomain : textdomain -> t -> t

(** get_textdomain () : Returns the current text domain.
*)
val get_textdomain : t -> textdomain

(** bindtextdomain textdomain dir : Set the default base directory for the specified
    domain.
*)
val bindtextdomain : textdomain -> dir -> t -> t

(** bind_textdomain_codeset textdomain codeset : Set the codeset to use for the
    specified domain. codeset must be a valid codeset for the underlying
    character encoder/decoder ( iconv, camomile, extlib... )
*)
val bindtextdomain_codeset : textdomain -> codeset -> t -> t
     
(** gettext t' str : Translate the string str.
*)
val gettext : t' -> string -> string

(** fgettext t' str : gettext returning format.
*)
val fgettext : t' -> string -> ('a, 'b, 'c, 'a) format4

(** dgettext t' textdomain str : Translate the string str for the specified domain.
*)
val dgettext : t' -> textdomain -> string -> string

(** fdgettext t' textdomain str : dgettext returning fformat.
*)
val fdgettext : t' -> textdomain -> string -> ('a, 'b, 'c, 'a) format4

(** dcgettext t' textdomain str category : Translate the string str for the specified
    domain and category.
*)
val dcgettext : t' -> textdomain -> string -> category -> string

(** fdcgettext t' textdomain str category : dcgettext returning fformat.
*)
val fdcgettext : t' -> textdomain -> string -> category -> ('a, 'b, 'c, 'a) format4

(** ngettext t' str str_plural n : Translate the string str using a plural form.
    str_plural is the default english plural. n is the relevant number for plural
    ( ie the number of objects deals with the string ).
*)
val ngettext : t' -> string -> string -> int -> string

(** fngettext t' str str_plural n : ngettext returning fformat.
*)
val fngettext : t' -> string -> string -> int -> ('a, 'b, 'c, 'a) format4

(** dngettext t' textdomain str str_plural n : Translate the string str using a plural
    form for the specified domain.
*)
val dngettext : t' -> textdomain -> string -> string -> int -> string

(** fdngettext t' textdomain str str_plural n : dngettext returning fformat.
*)
val fdngettext : t' -> textdomain -> string -> string -> int -> ('a, 'b, 'c, 'a) format4

(** dcngettext t' textdomain str str_plural n category : Translate the string str
    using a plural form for the specified domain and category.
*)
val dcngettext : t' -> textdomain -> string -> string -> int -> category -> string 

(** fdcngettext t' textdomain str str_plural n category : dcngettext returning
    fformat.
*)
val fdcngettext : t' -> textdomain -> string -> string -> int -> category -> ('a, 'b, 'c, 'a) format4


module type TRANSLATOR_TYPE = 
  functor ( Locale    : GettextLocale.LOCALE_TYPE ) ->
  functor ( Domain    : GettextDomain.DOMAIN_TYPE ) ->
  functor ( Charset   : GettextCharset.CHARSET_TYPE ) ->
  functor ( Translate : GettextTranslate.TRANSLATE_TYPE ) ->
  sig
    (** realize t : create a translator for all the value defined in t *)
    val realize : t -> t'
  end
;;

module type SIMPLE_TYPE =
  sig
    val s_ : t' -> string -> string 
    val f_ : t' -> string -> ('a, 'b, 'c, 'a) format4
    val sn_ : t' -> string -> string -> int -> string
    val fn_ : t' -> string -> string -> int -> ('a, 'b, 'c, 'a) format4
  end
;;

module type REF_SIMPLE_TYPE =
  sig
    val s_ : string -> string 
    val f_ : string -> ('a, 'b, 'c, 'a) format4
    val sn_ : string -> string -> int -> string
    val fn_ : string -> string -> int -> ('a, 'b, 'c, 'a) format4
  end
;;
