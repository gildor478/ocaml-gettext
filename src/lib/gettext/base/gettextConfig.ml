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

let default_dir = GettextConfigGen.default_localedir

let default_path () =
  let envpath =
    match Sys.getenv "OCAML_LOCALEPATH" with
    | s -> String.split_on_char ':' s
    | exception Not_found -> []
  in
  envpath @ [ GettextConfigGen.localedir; GettextConfigGen.default_localedir ]

let default_codeset = ""

let copyright =
  "ocaml-gettext v" ^ GettextConfigGen.version ^ "\n"
  ^ "Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>\n"
  ^ "Licenced under LGPL v2.1 with Ocaml exception"

let version = GettextConfigGen.version
