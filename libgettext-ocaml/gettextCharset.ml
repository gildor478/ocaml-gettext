(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
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
(**************************************************************************)

(** Signature of module for charset conversion 
    @author Sylvain Le Gall
  *)

open GettextTypes;;

module type CHARSET_TYPE = 
  sig
    type encoding = string
    type u
    
    (** create in_enc out_enc : create a new charset converter from charset 
        in_enc to out_enc.
    *)
    val create : t -> encoding -> encoding -> u

    (** recode str enc : return a transcoded string according to enc.
    *)
    val recode : u -> string  -> string
  end
;;

module Dummy : CHARSET_TYPE =
  struct
    type encoding = string
    type u = unit

    let create t in_enc out_enc = ()

    let recode () str = str
  end
;;
