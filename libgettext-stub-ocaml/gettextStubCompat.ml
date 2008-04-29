

(** Low level interface to gettext C library.
  * @author Sylvain Le Gall
  *)

type lc =
  | LC_CTYPE
  | LC_NUMERIC
  | LC_TIME
  | LC_COLLATE
  | LC_MONETARY
  | LC_MESSAGES
  | LC_ALL
;;

(** Set the current localization for the category
  *)
external setlocale : lc -> string -> string
	= "gettextStubCompat_setlocale"
;;

(** Look up MSGID in the current default message catalog for the current
  * LC_MESSAGES locale.  If not found, returns MSGID itself (the default text).
  *)
external gettext : string -> string
	= "gettextStubCompat_gettext"
;;

(** Look up MSGID in the DOMAINNAME message catalog for the current LC_MESSAGES
  * locale.
  *)
external dgettext : string -> string -> string
	= "gettextStubCompat_dgettext"
;;

(** Look up MSGID in the DOMAINNAME message catalog for the current CATEGORY
  * locale.
  *)
external dcgettext : string -> string -> lc -> string
	= "gettextStubCompat_dcgettext"
;;

(** Similar to `gettext' but select the plural form corresponding to the number
  * N.
  *)
external ngettext : string -> string -> int -> string
	= "gettextStubCompat_ngettext"
;;

(** Similar to `dgettext' but select the plural form corresponding to the number
  * N.
  *)
external dngettext : string -> string -> string -> int -> string
	= "gettextStubCompat_dngettext"
;;

(** Similar to `dcgettext' but select the plural form corresponding to the
  * number N.
  *)
external dcngettext : string -> string -> string -> int -> lc -> string
	= "gettextStubCompat_dcngettext"
;;

(** Set the current default message catalog to DOMAINNAME.If DOMAINNAME is "",
  * reset to the default of "messages".
  *)
external textdomain : string -> string
	= "gettextStubCompat_textdomain"
;;

(** Get the current default message catalog to DOMAINNAME.
  *)
external get_textdomain : unit -> string
	= "gettextStubCompat_get_textdomain"
;;

(** Specify that the DOMAINNAME message catalog will be foundin DIRNAME rather
  * than in the system locale data base.
  *)
external bindtextdomain : string -> string -> string
	= "gettextStubCompat_bindtextdomain"
;;

(** Specify the character encoding in which the messages from theDOMAINNAME
  * message catalog will be returned.
  *)
external bind_textdomain_codeset : string -> string -> string
	= "gettextStubCompat_bind_textdomain_codeset"
;;

