(** Implements different operation over locale/category *)

open GettextTypes;;
open GettextCategory;;

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
       * In the function dcgettext at every call the current setting of the 
       * highest priority environment variable is determined and used. 
       * Highest priority means here the following list with decreasing priority:
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
