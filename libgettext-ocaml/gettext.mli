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

(** Exception *)

val string_of_exception : exn -> string
;;

(** High level interfaces *)

(** Value of the dependencies for the initialization of the library 
    Gettext ( for translating exception and help message 
*)
val init : GettextTypes.dependencies
;;

(** Module to handle typical library requirement *)
module Library :
  functor ( Init : GettextTypes.Init ) ->
  sig
    val init  : GettextTypes.dependencies
    val s_    : string -> string 
    val f_    : string -> ('a, 'b, 'c, 'd) format4
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
