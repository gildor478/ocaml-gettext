(** Signature of module for domain management *)

open FilePath;;
open FilePath.DefaultPath;;
open FileUtil;;
open FileUtil.StrUtil;;
open GettextUtils;;

exception DomainFileDoesntExist of string * string;; 
exception DomainLanguageNotSet of string;;

module type DOMAIN_TYPE = 
  functor ( Locale : GettextLocale.LOCALE_TYPE ) ->
  sig
    type textdomain    = string
    type dir           = string
    type category      = Locale.category
    
    type t

    (** create language locale : create a new binding for textdomain
        considering. Language is guessed from locale environnement.
    *)
    val create : ?language : string -> Locale.t -> t

    (** add textdomain dir t : add the binding of textdomain to dir.
    *)
    val add : textdomain -> dir -> t -> t 

    (** compute_path textdomain category t : return the path to the 
        mo file corresponding to textdomain and category. Language is 
        guessed from category binding. If the textdomain is not found,
        it tries to use the build default to find the file. The file 
        returned exists and is readable. If such a file doesn't exists 
        an exception DomainFileDoesntExist is thrown. If the function is 
        unable to guess the current language an exception 
        DomainLanguageNotSet is thrown.
    *)
    val compute_path : textdomain -> category -> t -> dir
  end
;;

module Generic : DOMAIN_TYPE = 
  functor ( Locale : GettextLocale.LOCALE_TYPE ) ->
  struct    
    type textdomain  = string
    type dir         = string
    type category    = Locale.category

    type t = {
      locale   : Locale.t;
      language : string option;
      map      : dir MapString.t;
    }

    open Locale

    let guess_language domain category = 
      (* http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC155
       * In the function dcgettext at every call the current setting of the 
       * highest priority environment variable is determined and used. 
       * Highest priority means here the following list with decreasing priority:
          1. LANGUAGE
          2. LC_ALL
          3. LC_xxx, according to selected locale
          4. LANG 
      *)
      match domain.language with 
          Some str ->
            str
        | None ->
            try
              Sys.getenv "LANGUAGE" 
            with Not_found ->
              try
                get_locale all domain.locale
              with Not_found ->
                try 
                  get_locale category domain.locale
                with Not_found ->
                  try
                    Sys.getenv "LANG"
                  with Not_found ->
                    raise 
                    (DomainLanguageNotSet(string_of_category category))
    
    let create ?language locale = {
        locale   = locale;
        language = language; 
        map      = MapString.empty;
      }

    let add textdomain dir domain = {
        locale   = domain.locale;
        language = domain.language;
        map      = MapString.add textdomain dir domain.map 
      }

    let compute_path textdomain category domain = 
      let search_path = 
        try 
          MapString.find textdomain domain.map :: GettextConfig.default_paths
        with Not_found ->
          GettextConfig.default_paths
      in
      (* http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC148 
        dir_name/locale/LC_category/domain_name.mo *)
      let end_path = 
        make_filename [ 
          guess_language domain category; 
          string_of_category category ; 
          (* BUG : should use add_extension *)
          textdomain ^ ".mo" 
        ]
      in
      let compute_simple_path dir = 
        concat dir end_path
      in
      try
        List.find (test (And(Exists,Is_readable)))
        (List.map compute_simple_path search_path)
      with Not_found ->
        raise (DomainFileDoesntExist(textdomain,string_of_category category))
          
  end
;;
