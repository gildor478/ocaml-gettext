(** Signature of module for translation storage / access *)

open GettextTypes;;
open GettextUtils;;
open GettextMo;;

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
    val create : in_channel -> Charset.t -> t

    (** translate str (plural_form,number) tbl : translate the string 
        str using tbl. It is possible that the operation modify tbl, 
        so it is returned also. It is also possible to get the plural 
        form of the translated string using plural_form and number.
    *)
    val translate : 
         string 
      -> ?plural_form: (string * int)
      -> t 
      -> translated_type * t
  end
;;

module Dummy : TRANSLATE_TYPE =
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  struct
    type t = Charset.t

    let create chn charset = charset

    let translate str ?plural_form charset = 
      match plural_form with
        None 
      | Some(_,0) ->
          ( Singular(str,Charset.recode str charset), charset )
      | Some(str_plural,_) ->
          ( Plural
            (
              str,str_plural,
              [Charset.recode str charset ; Charset.recode str_plural charset]
            ), 
            charset
          )
  end
;;

module Map : TRANSLATE_TYPE =
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  struct
    type t = {
      charset   : Charset.t;
      mo_header : mo_header_type;
      chn       : in_channel;
      map       : translated_type MapString.t;
      last      : int;
    }

    let create chn charset = {
      charset   = charset;
      mo_header = input_mo_header chn;
      chn       = chn;
      map       = MapString.empty;
      last      = 0;
    }

    let rec translate str ?plural_form mp = 
      try 
        (MapString.find str mp.map, mp)
      with Not_found ->
        if mp.last < Int32.to_int mp.mo_header.number_of_strings then
          let new_translation = 
            input_mo_translation mp.chn mp.mo_header (mp.last + 1)
          in
          let new_map =
            match new_translation with
              Singular(id,str) ->
                MapString.add 
                str 
                (Singular(id,Charset.recode str mp.charset)) 
                mp.map
            | Plural(id,id_plural,lst) ->
                MapString.add
                str
                (Plural(id,id_plural,
                List.map (fun x -> Charset.recode x mp.charset) lst))
                mp.map
          in
          let new_mp = 
            {
              charset   = mp.charset;
              mo_header = mp.mo_header;
              chn       = mp.chn;
              map       = new_map;
              last      = mp.last + 1;
            }
          in
          match new_translation with
            Singular(id,_) when id = str ->
              (new_translation,new_mp)
          | Plural(id,_,_) when id = str ->
              (new_translation,new_mp)
          | _ ->
              (
                match plural_form with
                  Some x ->
                    translate str ~plural_form:x new_mp
                | None ->
                    translate str new_mp
              )
        else
          raise Not_found
            
  end
;;
