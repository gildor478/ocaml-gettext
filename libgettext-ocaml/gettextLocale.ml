(** Implements different operation over locale/category *)

open GettextTypes;;
open GettextCategory;;
open GettextUtils;;

module type LOCALE_TYPE = 
  sig
    (** get_locale t cat : Return the value of locale and encoding for cat. 
        The value returned is in ASCII. Priority should be given to the 
        values language/codeset provided in variable t.
    *)
    val get_locale : t -> category -> (locale list * codeset)
  end
;;

(** Return the best value of environnement variable, that can be found according to the
    priority defined in gettext. The choice take into account t and category,
    but may ignore it, if a variable with a best priority is set.
    This function can be used to get a value for a LOCALE_TYPE implementation.
    Raise Not_found if nothing appropriate.
*)
let posix_getenv t category = 
      (* http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC155
         In the function dcgettext at every call the current setting of the 
         highest priority environment variable is determined and used. 
         Highest priority means here the following list with decreasing priority:
          1. LANGUAGE
          2. LC_ALL
          3. LC_xxx, according to selected locale
          4. LANG 
      *)
      match t.language with 
        Some str -> 
          str
      | None ->
          let best_env = 
            List.find ( 
              fun s -> 
                try 
                  ignore(Sys.getenv s); 
                  true 
                with Not_found -> 
                  false 
              ) [ 
                "LANGUAGE" ; 
                string_of_category LC_ALL ; 
                string_of_category category;
                "LANG" ]
          in
          Sys.getenv best_env
;;

module Posix : LOCALE_TYPE =
  struct

    (* Extract from "man setlocale"
       A locale name is typically of  the  form  language[_territory][.codeset][@modi-
       fier],  where  language  is  an ISO 639 language code, territory is an ISO 3166
       country code, and codeset is  a  character  set  or  encoding  identifier  like
       ISO-8859-1 or UTF-8.  For a list of all supported locales, try "locale -a", cf.
       locale(1).
     *)
    
    let get_locale t category = 
      let posix_lang = posix_getenv t category
      in
      let locale = 
        try
          let lexbuf = 
            Lexing.from_string posix_lang
          in
          GettextLocale_parser.main 
          GettextLocale_lexer.token 
          lexbuf 
        with x ->
          fail_or_continue t.failsafe 
          (LocalePosixUnparseable (posix_lang^" "^(Printexc.to_string x)))
          (GettextLocale_types.create_locale posix_lang)
      in
      let locales = 
        match
        (locale.GettextLocale_types.territory,locale.GettextLocale_types.modifier) with
          Some territory, Some modifier ->
            [ 
              locale.GettextLocale_types.language^"_"^territory^"@"^modifier;
              locale.GettextLocale_types.language^"_"^territory;
              locale.GettextLocale_types.language
            ]
        | None, Some modifier ->
            [ 
              locale.GettextLocale_types.language^"@"^modifier;
              locale.GettextLocale_types.language 
            ]
        | Some territory, None ->
            [ 
              locale.GettextLocale_types.language^"_"^territory;
              locale.GettextLocale_types.language 
            ]
        | None, None ->
            [ 
              locale.GettextLocale_types.language 
            ]
      in
      let codeset = 
        match locale.GettextLocale_types.codeset with
          Some codeset ->
            codeset
        | None ->
            t.codeset
      in
      (locales,codeset)

  end
;;
