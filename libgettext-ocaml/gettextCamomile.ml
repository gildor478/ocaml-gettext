(** Concrete implementation based on camomile 
    ( @see <http://camomile.sourceforge.net/> Camomile library ) 
*)

module Charset : GettextCharset.CHARSET_TYPE =
  struct
    type encoding = string
    type t = {
      in_enc  : CharEncoding.t;
      out_enc : CharEncoding.t;
    }

    let create in_enc out_enc = {
      in_enc  = CharEncoding.of_name in_enc;
      out_enc = CharEncoding.of_name out_enc;
    }

    let recode str chrst = 
      CharEncoding.recode_string chrst.in_enc chrst.out_enc str
  end
;;

module Locale : GettextLocale.LOCALE_TYPE =
  struct
    type locale   = string
    type encoding = string
    type category = Locale.category
    type t = unit 

    let create () = ()
   
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

    let set_locale cat locale () =
      Locale.set_locale 
      cat 
      ~locale:locale 
      ~enc:(CharEncoding.name_of CharEncoding.ascii)

    let get_locale cat () =
      CharEncoding.recode_string 
      ~in_enc:CharEncoding.iso_c_locale
      ~out_enc:CharEncoding.ascii 
      (Locale.current_locale cat)

    let default_charset () =
      CharEncoding.current_encname ()

    let messages = Locale.LC_MESSAGES

    let all = Locale.LC_ALL
  end
;;

module Map : GettextModules.GETTEXT_TYPE =
 GettextModules.Generic(Locale(GettextDomain.Generic(Charset(GettextTranslate.Map))))
;;
  
