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
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  sig
    type t

    (** create chn charset : Create a translation table using chn as 
        the file descriptor for a mo file and charset as the charset 
        transcoder. If chn is closed, any subsequent action could
        failed ( ie, if the asked elements is cached, it could not 
        failed, for example ).
    *)
    val create : failsafe -> filename -> Charset.t -> t

    (** translate str (plural_form,number) tbl : translate the string 
        str using tbl. It is possible that the operation modify tbl, 
        so it is returned also. It is also possible to get the plural 
        form of the translated string using plural_form and number.
    *)
    val translate : 
         t
      -> string 
      -> (string * int) option
      -> translation option 
  end
;;

module Dummy : TRANSLATE_TYPE =
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  struct
    type t = Charset.t

    let create failsafe filename charset = charset

    let translate charset str plural_form = 
      match plural_form with
        None 
      | Some(_,0) ->
          Some(Singular(str,Charset.recode charset str))
      | Some(str_plural,_) ->
          Some(Plural(str,str_plural,[Charset.recode charset str; Charset.recode
          charset str_plural]))
  end
;;

module Map : TRANSLATE_TYPE =
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  struct
    module MapDummy = Dummy (Charset)
    
    type t = {
      dummy     : MapDummy.t;
      map       : translation MapString.t;
      failsafe  : failsafe;
    }

    let create failsafe filename charset =
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
              input_mo_translation failsafe chn mo_header i
            in
            map :=
              match new_translation with
                Singular(id,str) ->
                MapString.add id 
                (Singular(id, Charset.recode charset str)) 
                !map
              | Plural(id,id_plural,lst) ->
                MapString.add id 
                (Plural (id, id_plural, List.map (Charset.recode charset) lst)) 
                !map
          done
        with (Sys_error _) ->
          fail_or_continue failsafe
          string_of_exception
          (GettextTranslateCouldNotOpenFile filename)
          ()
      in
      {
        dummy    = MapDummy.create failsafe filename charset;
        map      = !map;
        failsafe = failsafe;
      }
      
    let translate t str plural_form =
      try
        Some(MapString.find str t.map)
      with Not_found ->
        fail_or_continue t.failsafe
        string_of_exception
        (GettextTranslateStringNotFound str)
        (MapDummy.translate t.dummy str plural_form)
  end
;;
