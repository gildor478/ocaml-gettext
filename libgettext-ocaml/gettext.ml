open GettextTypes;;

(* High level functions *)

module Library =
  functor ( Init : init ) ->
  functor ( InitDependencies : init list ) ->
  struct
    let textdomain = 
      let (textdomain,_,_) = Init 
      in
      textdomain

    let init = Init :: InitDependencies

    let s_   = dgettext (get_global_t' ()) textdomain 
    val f_   = fdgettext (get_global_t' ()) textdomain
    val sn_  = dngettext (get_global_t' ()) textdomain
    val fn_  = fdngettext (get_global_t' ()) textdomain
  end
;;

module Program = 
  functor ( Init : init ) ->
  functor ( InitDependencies : init list ) ->
  functor ( Realize : realize ) ->
  struct
    let textdomain = 
      let (textdomain,_,_) = Init
      in
      textdomain

    let init = [  
      (
        "--gettext-failsafe",
        ( Arg.Symbol 
          (
            ["ignore"; "inform-stderr"; "raise-exception"],
            ( fun 
                  "ignore"          -> set_global { get_global () with failsafe = Ignore }
                | "inform-stderr"   -> set_global { get_global () with failsafe = InformStderr }
                | "raise-exception" -> set_global { get_global () with failsafe = RaiseException }
            )
          )
        ),
        "Choose how to handle failure in gettext ( ignore, stderr, exception )" 
      );
      (
        "--gettext-disable",
        ( Arg.Unit 
          ( fun () -> set_global { get_global () with realize = XXX } 
          )
        ),
        "Disable the translation perform by gettext"
      );
      (
        "--gettext-domain-dir",
        ( Arg.Tuple 
          (
            Arg.String , Arg.String 
          )
        )
        "Set a dir to search gettext files for the specified domain"
      );
      (
        "--gettext-dir",
        ( Arg.String
          ( fun s -> set_global { get_global () with dir = s }
          )
        ),
        "Set the default dir to search gettext files"
      );
      (
        "--gettext-language",
        ( Arg.String
          ( fun s -> set_global { get_global () with language = s }
          )
        ),
        "Set the default language for gettext"
      );
      (
        "--gettext-codeset",
        ( Arg.String
          ( fun s -> set_global { get_global () with codeset = s }
          )
        ),
        "Set the default codeset for outputting string with gettext"
      );
      ]
      
    let s_   = dgettext (get_global_t' ()) textdomain 
    val f_   = fdgettext (get_global_t' ()) textdomain
    val sn_  = dngettext (get_global_t' ()) textdomain
    val fn_  = fdngettext (get_global_t' ()) textdomain

  end
;;
