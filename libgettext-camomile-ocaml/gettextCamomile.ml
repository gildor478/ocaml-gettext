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

module Locale : GettextLocale.LOCALE_TYPE =
  struct
    type locale   = string
    type encoding = string
    type category = Locale.category
    type t = failsafe

    let create failsafe = failsafe
   
    let compare_category c1 c2 = 
      let category_value c = 
        match c with 
          Locale.LC_ALL       -> 0 
        | Locale.LC_COLLATE   -> 1
        | Locale.LC_CTYPE     -> 2
        | Locale.LC_MONETARY  -> 3
        | Locale.LC_TIME      -> 4
        | Locale.LC_MESSAGES  -> 5
      in
      compare (category_value c1) (category_value c2)

    let string_of_category cat = 
      match cat with 
        Locale.LC_ALL       -> "LC_ALL"
      | Locale.LC_COLLATE   -> "LC_COLLATE"
      | Locale.LC_CTYPE     -> "LC_CTYPE"
      | Locale.LC_MONETARY  -> "LC_MONETARY"
      | Locale.LC_TIME      -> "LC_TIME"
      | Locale.LC_MESSAGES  -> "LC_MESSAGES"

    let set_locale cat locale failsafe =
      Locale.set_locale 
      cat 
      ~locale:locale 
      ~enc:(CharEncoding.name_of CharEncoding.ascii);
      failsafe

    let get_locale cat failsafe =
      CharEncoding.recode_string 
      ~in_enc:CharEncoding.iso_c_locale
      ~out_enc:CharEncoding.ascii 
      (Locale.current_locale cat)

    let default_charset failsafe =
      CharEncoding.current_encname ()

    let messages = Locale.LC_MESSAGES

    let all = Locale.LC_ALL
  end
;;

module Map : GettextModules.GETTEXT_TYPE =
 GettextModules.Generic 
 (Locale)                 (* Camomile locale *)
 (GettextDomain.Generic)  (* Generic domain *)
 (Charset)                (* Camomile charset *)
 (GettextTranslate.Map)   (* Generic translation *)
;;
  
