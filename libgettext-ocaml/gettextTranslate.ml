(** Signature of module for translation storage / access *)

open GettextTypes;;
open GettextUtils;;
open GettextMo;;

exception GettextTranslateCouldNotOpenFile of string ;;
exception GettextTranslateStringNotFound of string ;;

let string_of_exception exc =
  match exc with
    GettextTranslateCouldNotOpenFile fln ->
      "Could not open file "^fln
  | GettextTranslateStringNotFound str ->
      "Cannot find string "^str
  | _ ->
      ""
;;
          
module type TRANSLATE_TYPE =
  sig
    type u

    (** create t filename charset : Create a translation 
        table using filename as the mo file and charset as the charset 
        converter. 
    *)
    val create : t -> filename -> (string -> string) -> u

    (** translate str (plural_form,number) tbl : translate the string 
        str using tbl. It is possible that the operation modify tbl, 
        so it is returned also. It is also possible to get the plural 
        form of the translated string using plural_form and number.
    *)
    val translate : 
         u
      -> string 
      -> (string * int) option
      -> translation option 
  end
;;

module Dummy : TRANSLATE_TYPE =
  struct
    type u = string -> string

    let create t filename charset = charset

    let translate charset str plural_form = 
      match plural_form with
        None 
      | Some(_,0) ->
          Some(Singular(str,charset str))
      | Some(str_plural,_) ->
          Some(Plural(str,str_plural,[charset str;charset str_plural]))
  end
;;

module Map : TRANSLATE_TYPE =
  struct
    
    type u = {
      dummy     : Dummy.u;
      map       : translation MapString.t;
      failsafe  : failsafe;
    }

    let create t filename charset =
      let map = ref MapString.empty
      in
      let () = 
        try 
          let chn = open_in filename
          in
          let mo_header = input_mo_header chn
          in
          for i = 0 to Int32.to_int mo_header.number_of_strings
          do
            let new_translation = 
              input_mo_translation t.failsafe chn mo_header i
            in
            map :=
              match new_translation with
                Singular(id,str) ->
                MapString.add id 
                (Singular(id, charset str)) 
                !map
              | Plural(id,id_plural,lst) ->
                MapString.add id 
                (Plural (id, id_plural, List.map charset lst)) 
                !map
          done
        with (Sys_error _) ->
          fail_or_continue t.failsafe
          string_of_exception
          (GettextTranslateCouldNotOpenFile filename)
          ()
      in
      {
        dummy    = Dummy.create failsafe filename charset;
        map      = !map;
        failsafe = t.failsafe;
      }
      
    let translate u str plural_form =
      try
        Some(MapString.find str u.map)
      with Not_found ->
        fail_or_continue u.failsafe
        string_of_exception
        (GettextTranslateStringNotFound str)
        (Dummy.translate u.dummy str plural_form)
  end
;;
