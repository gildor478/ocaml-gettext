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

(** Concrete implementation based on camomile. 
    @see <http://camomile.sourceforge.net/> Camomile library
    @author Sylvain Le Gall
*)

(** {1 Concrete implementations} *)

(** Implementation based on a Map storage for string. 
  *)
module Map : GettextTypes.REALIZE_TYPE 
 
(** Implementation based on a Hashtbl storage for string. 
  *)
module Hashtbl : GettextTypes.REALIZE_TYPE 
 
(** Low memory and fast initialization implementation, files are opened only when needed. 
  *)
module Open : GettextTypes.REALIZE_TYPE 
  
