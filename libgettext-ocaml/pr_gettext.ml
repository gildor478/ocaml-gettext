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

(** Camlp4 dumper to extract strings.
    @author Sylvain Le Gall
    @author Richard W.M. Jones (translation to OCaml 3.10.X new camlp4)
  *)

(* Extract the string which should be used for a gettext translation. Output a
   po_content list through the function Marshal.to_channel
   Functions that are looked for :
Functions     Arg 1      Arg 2      Arg 3      Arg 4      Arg 5      Arg 6   ...
s_            singular
f_            singular
sn_           singular   plural     _
fn_           singular   plural     _
gettext       _          singular
fgettext      _          singular
dgettext      _          domain     singular
fdgettext     _          domain     singular
dcgettext     _          domain     singular   _
fdcgettext    _          domain     singular   _
ngettext      _          singular   plural     _        
fngettext     _          singular   plural     _          
dngettext     _          domain     singular   plural     _
fdngettext    _          domain     singular   plural     _
dcngettext    _          domain     singular   plural     _          _   
fdcngettext   _          domain     singular   plural     _          _


All this function name should also be matched when they are called using a
module.
                                      
*)

open Format
open GettextTypes
open GettextPo

let default_textdomain = ref None

module Id = struct
  (* name is printed with the -loaded-modules switch *) 
  let name = "pr_gettext" 
  (* cvs id's seem to be the preferred version string *) 
  let version = "$Id$" 
end 

module Make (Syntax : Camlp4.Sig.Camlp4Syntax)
  : Camlp4.Sig.Printer(Syntax.Ast).S =
struct
  module Loc = Syntax.Loc
  module Ast = Syntax.Ast

  type t = po_content

  let string_of_ocaml_string str =
    Scanf.sscanf 
      (Printf.sprintf "\"%s\"" str)
      "%S"
      (fun s -> s)

  let add_translation t loc ocaml_singular plural_opt domain =
    let filepos = 
      Loc.file_name loc, Loc.start_line loc 
    in
    let singular = 
      string_of_ocaml_string ocaml_singular
    in
    let translation =
      match plural_opt with 
        | Some plural_ocaml -> 
            let plural = 
              string_of_ocaml_string plural_ocaml
            in
              {
                po_comment_special = [];
                po_comment_filepos = [filepos];
                po_comment_translation = PoPlural([singular],[plural],[[""];[""]]);
              }
        | None -> 
            {
              po_comment_special = [];
              po_comment_filepos = [filepos];
              po_comment_translation = PoSingular([singular],[""]);
            }
    in
    match domain with 
      | Some domain -> 
          add_po_translation_domain domain t translation
      | None ->
          (
            match !default_textdomain with
              Some domain ->
                add_po_translation_domain domain t translation
            | None ->
                add_po_translation_no_domain t translation
          )

  let output_translations ?output_file m = 
    let (fd,close_on_exit) = 
      match output_file with
	Some f -> (open_out f,true)
      | None -> (stdout,false)
    in
    Marshal.to_channel fd m [];
    flush fd;
    if close_on_exit then
      close_out fd
    else
      ()

  (* Check if the given node belong to the given functions *)
  let is_like e functions = 
    let rec function_name e =
      match e with 
        | <:ident<$_$.$id:e$>> ->
          function_name e
        | <:ident<$lid:s$>> ->
          s
        | _ ->
            raise Not_found
    in
      try
        List.mem (function_name e) functions
      with Not_found ->
        false

  class visitor = object
    inherit Ast.fold as super

    val t = empty_po
    method t = t

    method expr = function
    | <:expr@loc< $id:e$ $str:singular$ >> when 
        is_like e ["s_"; "f_"] ->
      (* Add a singular / default domain string *)
      {< t = add_translation t loc singular None None >}

    | <:expr@loc< $id:e$ $str:singular$ $str:plural$ >> when 
        is_like e ["sn_"; "fn_"] ->
      (* Add a plural / default domain string *)
      {< t = add_translation t loc singular (Some plural) None >}

    | <:expr@loc< $id:e$ $expr$ $str:singular$ >> when 
        is_like e ["gettext"; "fgettext"] ->
      (* Add a singular / default domain string *)
      {< t = add_translation t loc singular None None >}

    | <:expr@loc< $id:e$ $expr$ $str:domain$ $str:singular$ >> when 
        is_like e ["dgettext"; "fdgettext"; "dcgettext"; "fdcgettext"] ->
      (* Add a singular / defined domain string *)
      {< t = add_translation t loc singular None (Some domain) >}

    | <:expr@loc< $id:e$ $expr$ $str:singular$ $str:plural$ >> when 
        is_like e ["ngettext"; "fngettext"] ->
      (* Add a plural / default domain string *)
      {< t = add_translation t loc singular (Some plural) None >}

    | <:expr@loc< $id:e$ $expr$ $str:domain$ $str:singular$ $str:plural$ >> when
        is_like e ["dngettext"; "fdngettext"; "dcngettext"; "fdcngettext"] ->
      (* Add a plural / defined domain string *)
      {< t = add_translation t loc singular (Some plural) (Some domain) >}

    | e -> super#expr e
  end

  (* Called on *.mli files. *)
  (* This was in the old code, but AFAICS interfaces can never
   * contain translatable strings (right??).  So I've changed this
   * to do nothing. - RWMJ 2008/03/21
   *)
  let print_interf ?input_file ?output_file _ = ()

  (* Called on *.ml files. *)
  let print_implem ?input_file ?output_file ast =
    let visitor = (new visitor)#str_item in
    let t = (visitor ast)#t in
    output_translations ?output_file t
end

(* Register the new printer. *)
module M = Camlp4.Register.OCamlPrinter(Id)(Make) ;;

(* XXX How to do this?
Pcaml.add_option "-default-textdomain" 
  (Arg.String ( fun textdomain -> default_textdomain := Some textdomain ) )
  "<textdomain> Defines the default textdomain"
;;
*)
