(** Signature of module for translation storage / access *)

open GettextTypes;;
open GettextUtils;;
open GettextMo;;
open GettextFormat;;

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

    (** create t filename recode : Create a translation 
        table using filename as the mo file and recode as the encoding
        converter. 
    *)
    val create : t -> filename -> (string -> string) -> u

    (* BUG : need update *)
    (** translate str (plural_form,number) tbl : translate the string 
        str using tbl. It is possible that the operation modify tbl, 
        so it is returned also. It is also possible to get the plural 
        form of the translated string using plural_form and number.
    *)
    val translate : 
         u
      -> bool
      -> string 
      -> (string * int) option
      -> string 
  end
;;

module Dummy : TRANSLATE_TYPE =
  struct
    type u = string -> string

    let create t filename charset = charset

    let translate charset printf_format str plural_form = 
      match plural_form with
        None ->
          charset str
      | Some(str_plural,x) ->
          let check =
            if printf_format then
              check_format Ignore
            else
              fun x -> x
          in
          charset (get_translated_value Ignore (check (Plural(str,str_plural,[]))) 
          (germanic_plural x))
  end
;;

module Map : TRANSLATE_TYPE =
  struct
    
    type u = {
      dummy            : Dummy.u;
      map              : translation MapString.t;
      failsafe         : failsafe;
      fun_plural_forms : int -> int;
    }

    let create t filename charset =
      let map = ref MapString.empty
      in
      let fun_plural_forms = 
        try 
          (* Processing of the file *)
          let chn = open_in filename
          in
          let mo_header = input_mo_header chn
          in
          let fun_plural_forms = 
            let informations = 
              input_mo_informations t.GettextTypes.failsafe chn mo_header
            in
            informations.GettextTypes.fun_plural_forms
          in
          for i = 0 to Int32.to_int mo_header.number_of_strings
          do
            let new_translation = 
              input_mo_translation t.GettextTypes.failsafe chn mo_header i
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
          done;
          fun_plural_forms
        with (Sys_error _) ->
          fail_or_continue t.GettextTypes.failsafe
          string_of_exception
          (GettextTranslateCouldNotOpenFile filename)
          germanic_plural
      in
      {
        dummy            = Dummy.create t filename charset;
        map              = !map;
        failsafe         = t.GettextTypes.failsafe;
        fun_plural_forms = fun_plural_forms;
      }
      
    let translate u printf_format str plural_form =
      try
        let plural_number = 
          u.fun_plural_forms (
            match plural_form with
              Some(_,x) -> x
              | None -> 0
          )
        in
        let check =
          if printf_format then
            check_format u.failsafe
          else
            fun x -> x
        in
        get_translated_value u.failsafe (check (MapString.find str u.map)) plural_number
      with Not_found ->
        fail_or_continue u.failsafe
        string_of_exception
        (GettextTranslateStringNotFound str)
        (Dummy.translate u.dummy printf_format str plural_form)
  end
;;
