exception GettextUninitialized;;

open FileUtil;;
open FileUtil.StrUtil;;
open GettextMo;;
open GettextTypes;;

(** Signature of module implementing gettext functions *)

module type GETTEXT_TYPE = 
  sig
    (** init categories language textdomain : Initialize the library globally.
        language should be a well formed ISO code one. If you use None
        for language, a value should be guessed using environnement, falling
        back to C default if guess failed. textdomain is the default catalog used 
        to lookup string. You can define more category binding usnig categories.
    *)
    val init : 
         ?categories : (GettextTypes.locale_category_type * string) list 
      -> ?language : string
      -> string 
      -> unit 

    (** close () : Close the current gettext working environnement.
    *)
    val close : unit -> unit
 
    (** textdomain domain : Set the current text domain.
    *)
    val textdomain : string -> unit

    (** get_textdomain () : Returns the current text domain.
    *)
    val get_textdomain : unit -> string

    (** bindtextdomain domain dir : Set the default base directory for the specified
        domain.
    *)
    val bindtextdomain : string -> string -> unit

    (** bind_textdomain_codeset domain codeset : Set the codeset to use for the
        specified domain. codeset must be a valid codeset for the underlying
        character encoder/decoder ( iconv, camomile, extlib... )
    *)
    val bind_textdomain_codeset : string -> string -> unit
     
    (** gettext str : Translate the string str.
    *)
    val gettext : string -> string

    (** dgettext domain str : Translate the string str for the specified domain.
    *)
    val dgettext : string -> string -> string

    (** dcgettext domain str category : Translate the string str for the specified
        domain and category.
    *)
    val dcgettext : string -> string -> GettextTypes.locale_category_type -> string

    (** ngettext str str_plural n : Translate the string str using a plural form.
        str_plural is the default english plural. n is the relevant number for plural
        ( ie the number of objects deals with the string ).
    *)
    val ngettext : string -> string -> int -> string

    (** dngettext domain str str_plural n : Translate the string str using a plural
        form for the specified domain.
    *)
    val dngettext : string -> string -> string -> int -> string

    (** dcngettext domain str str_plural n category : Translate the string str
        using a plural form for the specified domain and category.
    *)
    val dcngettext : string -> string -> string -> int -> GettextTypes.locale_category_type -> string 

  end
;;

module type META_GETTEXT_TYPE = 
  functor ( Domain  : GettextDomain.DOMAIN_TYPE ) ->
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  functor ( Translate : GettextTranslate.TRANSLATE_TYPE ) ->
    GETTEXT_TYPE
;;

module Generic : META_GETTEXT_TYPE = 
  functor ( Domain : GettextDomain.DOMAIN_TYPE ) ->
  functor ( Charset : GettextCharset.CHARSET_TYPE ) ->
  functor ( Translate : GettextTranslate.TRANSLATE_TYPE ) ->
  struct
    
    module Translate = Translate(Charset)

    type domain      = string
    type category    = GettextTypes.locale_category_type
    type translation = (in_channel * Translate.t) option
    
    module MapDomainCategory = Map.Make(struct
      type t      = domain * category
      let compare (d1,c1) (d2,c2) = 
        match String.compare d1 d2 with
          0 -> GettextCategory.compare c1 c2
        | x -> x
    end)

    type t = {
      domain       : Domain.t;
      textdomain   : string;
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
    
    let init ?(categories = []) ?(language) textdomain =
      let domain = 
        let default_domain = 
          match language with 
            Some lang ->
              (* Explicit set of the language *)
              Domain.create ~language:lang ()
          | None ->
              (* Try to guess the language *)
              Domain.create ()
        in
        List.fold_left 
        ( fun domain (cat,lang) -> Domain.add_category cat lang domain ) 
        default_domain categories
      in
      set_global_gettext {
        domain       = domain;
        textdomain   = textdomain;
        translations = MapDomainCategory.empty;
      }      

    let close () = 
      let gtxt = get_global_gettext ()
      in
      let close_one k v = 
        match v with
          Some(chn,_) -> close_in chn
        | None        -> ()
      in
      MapDomainCategory.iter close_one gtxt.translations;
      set_global_gettext {
        domain       = gtxt.domain;
        textdomain   = gtxt.textdomain;
        translations = MapDomainCategory.empty;
      }

    let textdomain textdomain =
      let gtxt = get_global_gettext ()
      in
      set_global_gettext {
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
        domain       = Domain.add_textdomain textdomain dir gtxt.domain;
        textdomain   = gtxt.textdomain;
        translations = gtxt.translations;
      }

    let bind_textdomain_codeset textdomain codeset = 
      let gtxt = get_global_gettext ()
      in
      set_global_gettext {
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
          let new_translation = 
            let new_mo_file = 
              Domain.compute_path textdomain category gtxt.domain
            in
            if test (And(Exists,Is_readable)) new_mo_file then
              let chn = open_in_bin new_mo_file 
              in
              let mo_header = input_mo_header chn
              in
              let mo_informations = input_mo_informations chn mo_header
              in
              (* BUG : we don't check that there is a hook to prevent
                 recoding of string. *)
              let charset = Charset.create_default mo_informations.content_type_charset
              in
              Some (chn, Translate.create chn charset)
            else
              None
          in
          new_translation
      in
      (* Really translate *)
      let (result,new_translation) = 
        match (translation,str,plural_form) with
          (None,res,None)     
        | (None,res,Some(_,0)) 
        | (None,_,Some(res,_)) -> 
            (res,None)
        | (Some(chn,tbl),str,None) -> 
            let (res,new_tbl) = Translate.translate str tbl
            in
            (res, Some(chn,new_tbl))
        | (Some(chn,tbl),str,Some(str_plural,n)) -> 
            let (res,new_tbl) = Translate.translate str ~plural_form:(str_plural,n) tbl
            in
            (res, Some(chn,new_tbl))
      in
      (* Reinject the changed entry *)
      set_global_gettext {
          domain       = gtxt.domain;
          textdomain   = gtxt.textdomain;
          translations = MapDomainCategory.add (textdomain,category) new_translation gtxt.translations;
      };
      result
          
    let gettext str =
      translate (get_textdomain ()) str LC_MESSAGES

    let dgettext domain str =
      translate domain str LC_MESSAGES

    let dcgettext domain str category = 
      translate domain str category

    let ngettext str str_plural n =
      translate (get_textdomain ()) str ~plural_form:(str_plural,n) LC_MESSAGES

    let dngettext domain str str_plural n =
      translate domain str ~plural_form:(str_plural,n) LC_MESSAGES

    let dcngettext domain str str_plural n category = 
      translate domain str ~plural_form:(str_plural,n) category
      
  end
;;
