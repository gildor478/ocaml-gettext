open FileUtil;;
open FileUtil.StrUtil;;
open GettextMo;;
open GettextTypes;;

(** Exceptions *)

exception GettextUninitialized;;
exception GettextMoFileNotFound;;
exception GettextNoTranslation of string;;

let string_of_exception exc =
  match exc with
    GettextUninitialized -> 
      "Gettext is not initialized"
  | GettextMoFileNotFound -> 
      "Gettext could not find the corresponding mo file"
  | GettextNoTranslation str_id -> 
      "Gettext could not find translation for string id \""^str_id^"\""
  | _ ->
      ""
;;


module type META_GETTEXT_TYPE = 
  functor ( Locale    : GettextLocale.LOCALE_TYPE ) ->
  functor ( Domain    : GettextDomain.DOMAIN_TYPE ) ->
  functor ( Charset   : GettextCharset.CHARSET_TYPE ) ->
  functor ( Translate : GettextTranslate.TRANSLATE_TYPE ) ->
    ( GETTEXT_TYPE with type category = Locale.category )
;;

module Generic : META_GETTEXT_TYPE = 
  functor ( Locale    : GettextLocale.LOCALE_TYPE ) ->
  functor ( Domain    : GettextDomain.DOMAIN_TYPE ) ->
  functor ( Charset   : GettextCharset.CHARSET_TYPE ) ->
  functor ( Translate : GettextTranslate.TRANSLATE_TYPE ) ->
  struct
    
    module Translate = Translate(Charset)

    module Domain = Domain(Locale)
    
    (* Public type *)
    type textdomain = string
    type locale     = string
    type dir        = string
    type codeset    = string
    type category   = Locale.category
    
    (* Hide type *)
    type translation = (string * in_channel * mo_translation_type * Translate.t) option
    
    module MapDomainCategory = Map.Make(
      struct
        type t      = textdomain * category
        
        let compare (d1,c1) (d2,c2) = 
          match String.compare d1 d2 with
            0 -> Locale.compare_category c1 c2
          | x -> x
      end)

    type t = {
      failsafe     : failsafe;
      locale       : Locale.t;
      domain       : Domain.t;
      textdomain   : textdomain;
      translations : translation MapDomainCategory.t;
    }
    
    (* Global variable for gettext -- i know that global variable is not good !  *)
    let global_gettext = ref None

    let get_global_gettext () = 
      match !global_gettext with
        Some x -> x
      | None -> raise GettextUninitialized

    let set_global_gettext gtxt = 
      global_gettext := Some gtxt
    
    let format_of_string = Obj.magic 
      
    let fail_or_continue failsafe exc cont_value =
      GettextUtils.fail_or_continue failsafe string_of_exception exc cont_value
      
    let init ?(failsafe = Ignore) ?(categories = []) ?(language) textdomain =
      let locale =
          List.fold_left
          ( fun locales (cat,locale) -> Locale.set_locale cat locale locales ) 
          (Locale.create failsafe) categories
      in
      let domain = 
        match language with 
          Some lang ->
            (* Explicit set of the language *)
            Domain.create ~language:lang failsafe locale 
        | None ->
            (* Try to guess the language *)
            Domain.create failsafe locale
      in
      set_global_gettext {
        failsafe     = failsafe;
        locale       = locale;
        domain       = domain;
        textdomain   = textdomain;
        translations = MapDomainCategory.empty;
      }

    let reinit ?(categories = []) ?(language) textdomain =
      (* BUG : the reinit could imply that the output charset
         has also changed, it should be checked ! ( for example
         before, the output charset was UTF-8 now it has switch
         to UTF-16... We should recode translations. *)
      let old_gtxt = get_global_gettext ()
      in
      let new_gtxt = 
        let _ = 
          match language with
            Some lang ->
              init ~language:lang ~categories:categories ~failsafe:old_gtxt.failsafe textdomain
          | None ->
              init ~categories:categories ~failsafe:old_gtxt.failsafe textdomain
        in
        get_global_gettext ()
      in
      let still_actual_files = 
        let add_if_actual (textdomain,category) value new_map = 
          match value with
            None ->
              new_map
          | Some (old_file,chn,mo_informations,translate) ->
              try
                let new_file = 
                  Domain.compute_path textdomain category new_gtxt.domain
                in
                let already_there = 
                  MapDomainCategory.mem (textdomain,category) new_map 
                in
                if new_file = old_file && not already_there then
                  MapDomainCategory.add (textdomain,category) value new_map
                else
                  (
                    close_in chn;
                    new_map
                  )
              with 
                GettextDomain.DomainFileDoesntExist _ 
              | GettextDomain.DomainLanguageNotSet _ ->
                  (
                    close_in chn;
                    fail_or_continue old_gtxt.failsafe GettextMoFileNotFound new_map;
                  )
        in        
        MapDomainCategory.fold add_if_actual old_gtxt.translations new_gtxt.translations
      in
      set_global_gettext {
        failsafe     = new_gtxt.failsafe;
        locale       = new_gtxt.locale;
        domain       = new_gtxt.domain;
        textdomain   = new_gtxt.textdomain;
        translations = still_actual_files;
      }

    let close () = 
      let gtxt = get_global_gettext ()
      in
      let close_one k v = 
        match v with
          Some(_,chn,_,_) -> close_in chn
        | None        -> ()
      in
      MapDomainCategory.iter close_one gtxt.translations;
      set_global_gettext {
        failsafe     = gtxt.failsafe;
        locale       = gtxt.locale;
        domain       = gtxt.domain;
        textdomain   = gtxt.textdomain;
        translations = MapDomainCategory.empty;
      }

    let textdomain textdomain =
      let gtxt = get_global_gettext ()
      in
      set_global_gettext {
        failsafe     = gtxt.failsafe;
        locale       = gtxt.locale;
        domain       = gtxt.domain;
        textdomain   = textdomain;
        translations = gtxt.translations;
      }

    let get_textdomain () = 
      let gtxt = get_global_gettext ()
      in
      gtxt.textdomain

    let bindtextdomain textdomain dir =
      let gtxt = get_global_gettext ()
      in
      set_global_gettext {
        failsafe     = gtxt.failsafe;
        locale       = gtxt.locale;
        domain       = Domain.add textdomain dir gtxt.domain;
        textdomain   = gtxt.textdomain;
        translations = gtxt.translations;
      }

    let bind_textdomain_codeset textdomain codeset = 
      (* BUG : does nothing -- for now *)
      let gtxt = get_global_gettext ()
      in
      set_global_gettext {
        failsafe     = gtxt.failsafe;
        locale       = gtxt.locale;
        domain       = gtxt.domain;
        textdomain   = gtxt.textdomain;
        translations = gtxt.translations;
      }

    let translate textdomain str ?(plural_form) category = 
      let gtxt = get_global_gettext ()
      in
      (* First of all, try to find the translation type *)
      let translation = 
        try 
          MapDomainCategory.find (textdomain,category) gtxt.translations
        with Not_found ->
          try 
            let new_mo_file = 
              Domain.compute_path textdomain category gtxt.domain
            in
            let chn = open_in_bin new_mo_file 
            in
            let mo_header = input_mo_header chn
            in
            let mo_informations = input_mo_informations gtxt.failsafe chn mo_header
            in
            (* BUG : we don't check that there is a specific which could prevent
               recoding of string. *)
            let charset = 
              Charset.create 
              gtxt.failsafe
              mo_informations.content_type_charset
              (Locale.default_charset gtxt.locale)
            in
            Some (new_mo_file, chn, mo_informations,Translate.create gtxt.failsafe chn charset)
          with GettextDomain.DomainFileDoesntExist _ 
          | GettextDomain.DomainLanguageNotSet _ ->
            fail_or_continue gtxt.failsafe GettextMoFileNotFound None
      in
      (* Really translate *)
      let (result,new_translation) = 
        match (translation,str,plural_form) with
          (None,res,None) ->
            (res,None)
        | (None,str,Some(str_plural,n)) ->
            if germanic_plural n = 0 then 
              (str,None)
            else 
              (str_plural,None)
        | (Some(file,chn,info,tbl),str,Some(str_plural,n)) -> 
          (
            let (res,new_tbl) = Translate.translate str tbl
            in
            let new_str =
              match res with 
                Some trsl ->
                  get_translated_value gtxt.failsafe trsl (info.fun_plural_forms n)
              | None ->
                  fail_or_continue gtxt.failsafe 
                  (GettextNoTranslation str)
                  (
                    if info.fun_plural_forms n = 0 then
                      str
                    else
                      str_plural
                  )
              in
              (new_str,Some(file, chn, info, new_tbl))
            )
        | (Some(file,chn,info,tbl),str,None) ->
            (
              let (res,new_tbl) = Translate.translate str tbl
              in
              let new_str =
                match res with 
                  Some trsl ->
                    get_translated_value gtxt.failsafe trsl 0
                | None ->
                    fail_or_continue gtxt.failsafe
                    (GettextNoTranslation str)
                    str
              in
              (new_str, Some(file, chn, info, new_tbl))
            )
      in
      (* Reinject the changed entry *)
      set_global_gettext {
          failsafe     = gtxt.failsafe;
          locale       = gtxt.locale;
          domain       = gtxt.domain;
          textdomain   = gtxt.textdomain;
          translations = MapDomainCategory.add (textdomain,category) new_translation gtxt.translations;
      };
      result 
          
  end
;;
