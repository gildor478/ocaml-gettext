(**************************************************************************)
(*  Ocaml-gettext : a library to translate messages                       *)
(*                                                                        *)
(*  Copyright (C) 2003, 2004, 2005 Sylvain Le Gall <sylvain@le-gall.net>  *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(*                                                                        *)
(*  Contact: sylvain@le-gall.net                                          *)
(**************************************************************************)

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
    val fn_   : string -> string -> int -> ('a, 'b, 'c, 'd) format4
  end
;;    

(** Module to handle typical program requirement *)
module Program :
  functor ( Init : GettextTypes.InitProgram ) ->
  sig
    val init  : (Arg.key * Arg.spec * Arg.doc ) list * string 
    val s_    : string -> string 
    val f_    : string -> ('a, 'b, 'c, 'd) format4
    val sn_   : string -> string -> int -> string
    val fn_   : string -> string -> int -> ('a, 'b, 'c, 'd) format4
  end
;;
