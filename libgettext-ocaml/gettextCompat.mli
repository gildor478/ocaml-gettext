(** Gettext compatibility functions ( use this one to have the closest API to gettext *)
open GettextTypes

(** textdomain domain t : Set the current text domain.
*)
val textdomain : textdomain -> t -> t

(** get_textdomain t : Returns the current text domain.
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
val bind_textdomain_codeset : textdomain -> codeset -> t -> t
     
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

(** fdngettext t' textdomain str str_plural n : dngettext returning format.
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


