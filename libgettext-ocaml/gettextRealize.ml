(** Module type for the function realize *)

module type REALIZE_TYPE =
  sig
    val realize : realize
  end
;;

module type Generic : REALIZE_TYPE =
  functor ( Translate : GettextTranslate.TRANSLATE_TYPE )
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  functor ( Locale : GettextLocale.LOCALE_TYPE ) ->
  struct

    let realize t = 
      let create_texdomain_category textdomain category =
        let codeset = 
          try
            match (MapTextdomain.find t.textdomains textdomain) with
              (Some codeset, _) -> codeset
            | None -> Locale.get_locale t category 
          with Not_found ->
            Locale.get_locale t category
        in
        let filename = 
          GettextDomain.compute_path 
          t 
          (fst (Locale.get_locale t category)) 
          category 
          textdomain
        in

        
    
  end
;;
      
