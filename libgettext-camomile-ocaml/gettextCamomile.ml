(** Concrete implementation based on camomile 
    ( @see <http://camomile.sourceforge.net/> Camomile library ) 
*)

open Camomile;;
open GettextTypes;;

module Charset : GettextCharset.CHARSET_TYPE =
  struct
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

(* BUG : i am not sure i could rely on ocamli18n, it is based on LDML, as we
   used POSIX convention *)
module Locale : GettextLocale.LOCALE_TYPE = 
  struct
    
    let get_locale t category =
      let locale_lst = 
        let value = GettextLocale.posix_getenv t category
        in
        let i18n = I18N.Locale.make_posix value
        in
        let i18n_lang_country = { 
          I18N.Locale.root with 
          I18N.Locale.territory = i18n.I18N.Locale.territory;
          I18N.Locale.language = i18n.I18N.Locale.language;
        }
        in
        let i18n_lang = {
          I18N.Locale.root with
          I18N.Locale.language = i18n.I18N.Locale.language
        }
        in
        [ I18N.Locale.to_string i18n_lang_country ; I18N.Locale.to_string i18n_lang ]
      in
      let codeset = 
        (* BUG: with have got problem trying to fetch the locale system codeset, hope it
           is provided through t *)
        t.codeset
      in
      (locale_lst,codeset)
  end
;;

module Map : GettextRealize.REALIZE_TYPE =
 GettextRealize.Generic 
 (GettextTranslate.Map)   (* Generic translation *)
 (Charset)                (* Camomile charset *)
 (Locale)                 (* Ocamli18N locale *)
;;
  
