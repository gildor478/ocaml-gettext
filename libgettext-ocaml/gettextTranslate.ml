(** Signature of module for translation storage / access *)

open GettextTypes;;

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
