(** Gettext functions *)
(** This module defines all the function required to use gettext. The primary
* design is to use applicative function. The "side effect" of such a choice is
* that you must defines, before using any function, all the text domains,
* codeset et al. When building a library, you should define a function
* "gettext_init" that will bind all the required stuff.
* The only function missing here is the "realize" function. This function is
* defined in the real implementation library provided with this modules ( see
* GettextDummy, GettextCamomile and GettextStub ).
**)

(** Exceptions *)

exception GettextUninitialized;;

val string_of_exception : exn -> string

(** Implementation of gettext functions *)

(* create failsafe categories codesets dirs language textdomain : Initialize the library globally.
    language should be a well formed ISO code one. If you don't set
    the language, a value should be guessed using environnement, falling
    back to C default if guess failed. textdomain is the default catalog used 
    to lookup string. You can define more category binding using categories.
    You can define codeset on a per domain basis.
*)
(*val create : 
     ?failsafe : failsafe  
  -> ?categories : (category * locale) list 
  -> ?codesets : (textdomain * codeset) list
  -> ?dirs : (textdomain * dir) list
  -> ?textdomains : textdomain list
  -> ?codeset : codeset
  -> ?language : locale
  -> textdomain 
  -> t
*)
(** High level functions *)

(** Module to handle typical library requirement *)
module Library :
  functor ( Init : GettextTypes.Init ) ->
  sig
    val init  : GettextTypes.dependencies
    val s_    : string -> string 
    val f_    : string -> ('a, 'b, 'c, 'a) format4
    val sn_   : string -> string -> int -> string
    val fn_   : string -> string -> int -> ('a, 'b, 'c, 'a) format4
  end
;;    

(** Module to handle typical program requirement *)
module Program :
  functor ( Init : GettextTypes.InitProgram ) ->
  sig
    val init  : (Arg.key * Arg.spec * Arg.doc ) list * string 
    val s_    : string -> string 
    val f_    : string -> ('a, 'b, 'c, 'a) format4
    val sn_   : string -> string -> int -> string
    val fn_   : string -> string -> int -> ('a, 'b, 'c, 'a) format4
  end
;;
