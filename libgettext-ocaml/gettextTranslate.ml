(** Signature of module for translation storage / access *)

open GettextTypes;;
open GettextUtils;;
open GettextMo;;
open GettextFormat;;

exception GettextTranslateStringNotFound of string ;;

let string_of_exception exc =
  match exc with
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
      let (map,fun_plural_forms) = 
        fold_mo 
        t.GettextTypes.failsafe
        ( fun translation accu ->
          match translation with
            Singular(str_id, str) ->
              MapString.add str_id
              (Singular(str_id,charset str))
              accu
          | Plural(str_id,str_plural,lst) ->
              MapString.add str_id
              (Plural(str_id,str_plural,List.map charset lst))
              accu
        )
        MapString.empty
        filename
      in
      {
        dummy            = Dummy.create t filename charset;
        map              = map;
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

module Hashtbl : TRANSLATE_TYPE =
  struct
    
    type u = {
      dummy            : Dummy.u;
      hashtbl          : (string,translation) Hashtbl.t;
      failsafe         : failsafe;
      fun_plural_forms : int -> int;
    }

    let create t filename charset =
      let (hashtbl,fun_plural_forms) = 
        fold_mo 
        t.GettextTypes.failsafe
        ( fun translation accu ->
          match translation with
            Singular(str_id, str) ->
              (
                Hashtbl.add accu str_id
                (Singular(str_id,charset str));
                accu
              )
          | Plural(str_id,str_plural,lst) ->
              (
                Hashtbl.add accu str_id
                (Plural(str_id,str_plural,List.map charset lst));
                accu
              )
        )
        (* 32 is only a guest on the number of string contains in the 
           future table *)
        (Hashtbl.create 32)
        filename
      in
      {
        dummy            = Dummy.create t filename charset;
        hashtbl          = hashtbl;
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
        get_translated_value u.failsafe (check (Hashtbl.find u.hashtbl str)) plural_number
      with Not_found ->
        fail_or_continue u.failsafe
        string_of_exception
        (GettextTranslateStringNotFound str)
        (Dummy.translate u.dummy printf_format str plural_form)
  end
;;

module Open : TRANSLATE_TYPE =
  struct
    
    type u = {
      dummy             : Dummy.u;
      filename          : filename;
      charset           : string -> string;
      failsafe          : failsafe;
      fun_plural_forms  : int -> int;
      number_of_strings : int;
    }

    let create t filename charset =
      (* Processing of the file *)
      let chn =
        open_in filename
      in
      let mo_header = input_mo_header chn
      in
      let informations = 
        input_mo_informations t.GettextTypes.failsafe chn mo_header
      in
      let fun_plural_forms = 
        informations.GettextTypes.fun_plural_forms
      in
      {
        dummy            = Dummy.create t filename charset;
        filename         = filename;
        charset          = charset;
        failsafe         = t.GettextTypes.failsafe;
        fun_plural_forms = fun_plural_forms;
        number_of_strings= 
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
        get_translated_value u.failsafe (check (Hashtbl.find u.hashtbl str)) plural_number
      with Not_found ->
        fail_or_continue u.failsafe
        string_of_exception
        (GettextTranslateStringNotFound str)
        (Dummy.translate u.dummy printf_format str plural_form)
  end
;
