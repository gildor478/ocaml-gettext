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

(** Signature of module implementing gettext functions *)

module type GETTEXT_TYPE = 
  sig

    type textdomain = string
    type locale     = string
    type dir        = string
    type codeset    = string
    type category 
    
    (** init failsafe categories language textdomain : Initialize the library globally.
        language should be a well formed ISO code one. If you don't set
        the language, a value should be guessed using environnement, falling
        back to C default if guess failed. textdomain is the default catalog used 
        to lookup string. You can define more category binding using categories.
    *)
    val init : 
         ?failsafe : failsafe  
      -> ?categories : (category * locale) list 
      -> ?language : locale
      -> textdomain 
      -> unit 

    (** reinit failsafe categories language textdomain : Reinitialize the library 
        globally. Argument have the same meaning as init
      *)
    val reinit : 
         ?categories : (category * locale) list
      -> ?language : locale
      -> textdomain
      -> unit
      
    (** close () : Close the current gettext working environnement.
    *)
    val close : unit -> unit
 
    (** textdomain domain : Set the current text domain.
    *)
    val textdomain : textdomain -> unit

    (** get_textdomain () : Returns the current text domain.
    *)
    val get_textdomain : unit -> textdomain

    (** bindtextdomain textdomain dir : Set the default base directory for the specified
        domain.
    *)
    val bindtextdomain : textdomain -> dir -> unit

    (** bind_textdomain_codeset textdomain codeset : Set the codeset to use for the
        specified domain. codeset must be a valid codeset for the underlying
        character encoder/decoder ( iconv, camomile, extlib... )
    *)
    val bind_textdomain_codeset : textdomain -> codeset -> unit
     
    (** gettext str : Translate the string str.
    *)
    val gettext : string -> string
    
    (** fgettext str : gettext returning format.
    *)
    val fgettext : string -> ('a, 'b, 'c, 'a) format4

    (** dgettext textdomain str : Translate the string str for the specified domain.
    *)
    val dgettext : textdomain -> string -> string

    (** fdgettext textdomain str : dgettext returning fformat.
    *)
    val fdgettext : textdomain -> string -> ('a, 'b, 'c, 'a) format4
    
    (** dcgettext textdomain str category : Translate the string str for the specified
        domain and category.
    *)
    val dcgettext : textdomain -> string -> category -> string

    (** fdcgettext textdomain str category : dcgettext returning fformat.
    *)
    val fdcgettext : textdomain -> string -> category -> ('a, 'b, 'c, 'a) format4
    
    (** ngettext str str_plural n : Translate the string str using a plural form.
        str_plural is the default english plural. n is the relevant number for plural
        ( ie the number of objects deals with the string ).
    *)
    val ngettext : string -> string -> int -> string

    (** fngettext str str_plural n : ngettext returning fformat.
    *)
    val fngettext : string -> string -> int -> ('a, 'b, 'c, 'a) format4
    
    (** dngettext textdomain str str_plural n : Translate the string str using a plural
        form for the specified domain.
    *)
    val dngettext : textdomain -> string -> string -> int -> string

    (** fdngettext textdomain str str_plural n : dngettext returning fformat.
    *)
    val fdngettext : textdomain -> string -> string -> int -> ('a, 'b, 'c, 'a) format4
    
    (** dcngettext textdomain str str_plural n category : Translate the string str
        using a plural form for the specified domain and category.
    *)
    val dcngettext : textdomain -> string -> string -> int -> category -> string 

    (** fdcngettext textdomain str str_plural n category : dcngettext returning
        fformat.
    *)
    val fdcngettext : textdomain -> string -> string -> int -> category -> ('a, 'b, 'c, 'a) format4
  end
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
          
    let gettext str =
      translate (get_textdomain ()) str Locale.messages

    let fgettext (str : string) =
      format_of_string (gettext str)
      
    let dgettext domain str =
      translate domain str Locale.messages

    let fdgettext domain str =
      format_of_string (dgettext domain str)
      
    let dcgettext domain str category = 
      translate domain str category

    let fdcgettext domain str category =
      format_of_string (dcgettext domain str category)
      
    let ngettext str str_plural n =
      translate (get_textdomain ()) str ~plural_form:(str_plural,n) Locale.messages

    let fngettext str str_plural n =
      format_of_string (ngettext str str_plural n)
      
    let dngettext domain str str_plural n =
      translate domain str ~plural_form:(str_plural,n) Locale.messages

    let fdngettext domain str str_plural n =
      format_of_string (dngettext domain str str_plural n)
      
    let dcngettext domain str str_plural n category = 
      translate domain str ~plural_form:(str_plural,n) category
      
    let fdcngettext domain str str_plural n category =
      format_of_string (dcngettext domain str str_plural n category)
  end
;;
