(**************************************************************************)
(*  ocaml-gettext : a library to translate messages                       *)
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


open CamomileLibrary;;
open Default.Camomile;;
open GettextTypes;;

(** Charset module, that is derived directly from the camomile library. *)
module Charset : GettextCharset.CHARSET_TYPE =
  struct
    (**/**)
    type encoding = string
    type u = {
      failsafe : failsafe;
      in_enc   : CharEncoding.t;
      out_enc  : CharEncoding.t;
    }

    let create t in_enc out_enc = {
      failsafe = t.GettextTypes.failsafe;
      in_enc   = CharEncoding.of_name in_enc;
      out_enc  = CharEncoding.of_name out_enc;
    }

    let recode u str = 
      CharEncoding.recode_string u.in_enc u.out_enc str
  end
;;

(** Implementation based on a Map storage for string. *)
module Map : GettextTypes.REALIZE_TYPE =
  GettextRealize.Generic 
  (GettextTranslate.Map)     (* Map translation *)
  (Charset)                  (* Camomile charset *)
  (GettextLocale.Posix)      (* POSIX locale *)
;;
 
(** Implementation based on a Hashtbl storage for string. *)
module Hashtbl : GettextTypes.REALIZE_TYPE =
  GettextRealize.Generic 
  (GettextTranslate.Hashtbl) (* Hashtbl translation *)
  (Charset)                  (* Camomile charset *)
  (GettextLocale.Posix)      (* POSIX locale *)
;;
 
(** Low memory and fast initialization implementation, files are opened only when needed. 
 *)
module Open : GettextTypes.REALIZE_TYPE =
  GettextRealize.Generic 
  (GettextTranslate.Open)    (* Open translation *)
  (Charset)                  (* Camomile charset *)
  (GettextLocale.Posix)      (* POSIX locale *)
;;
  
