(** Concrete implementation based on native gettext library 
    ( @see <http://www.gnu.org/software/gettext/gettext.html/> Gettext library ) 
*)

open GettextTypes;;
open GettextUtils;;

(** Native implementation of gettext. All translation are bound to C library
    call. Still use check_format, to ensure that strings follow printf format.
  *)
module Native : GettextRealize.REALIZE_TYPE =
  struct

    let realize t =
      (* Here we do the binding between C library call and the information we
         have in parameter t. *)
      let native_category_of_category cat =
        match cat with
          GettextTypes.LC_CTYPE    ->  GettextStubCompat.LC_CTYPE    
        | GettextTypes.LC_NUMERIC  ->  GettextStubCompat.LC_NUMERIC  
        | GettextTypes.LC_TIME     ->  GettextStubCompat.LC_TIME     
        | GettextTypes.LC_COLLATE  ->  GettextStubCompat.LC_COLLATE  
        | GettextTypes.LC_MONETARY ->  GettextStubCompat.LC_MONETARY 
        | GettextTypes.LC_MESSAGES ->  GettextStubCompat.LC_MESSAGES 
        | GettextTypes.LC_ALL      ->  GettextStubCompat.LC_ALL      
      in
      let default_dir = 
        match t.path with
         default_dir :: _ ->
           Some default_dir 
        | [] ->
            None
      in
      let bind_textdomain_one textdomain (codeset_opt,dir_opt) =
        (
          let codeset =
            match codeset_opt with 
              Some codeset ->
                codeset
            | None ->
                t.codeset
          in
          let str : string = 
            GettextStubCompat.bind_textdomain_codeset textdomain codeset
          in
          ()
        );
        (
          match dir_opt with
            Some dir ->
              let str : string =
                GettextStubCompat.bindtextdomain textdomain dir
              in
              ()
          | None ->
              (
                match default_dir with
                  Some dir ->
                    let str : string = 
                      GettextStubCompat.bindtextdomain textdomain dir
                    in
                    ()
                | None ->
                    ()
              )
        )
      in
      (* We only use the first path of t.path, since there is no notion of search path 
         in native gettext. So the MO file should be in :
           - first component of t.path,
           - directory pointed by bindtextdomain,
           - default directory of gettext.
       *)
      let str : string = 
        GettextStubCompat.textdomain t.default
      in
      let old_language : string = 
          match t.language with
            Some language ->
              (
                try
                  GettextStubCompat.setlocale GettextStubCompat.LC_ALL language
                with Failure("setlocale(invalid localization)" as str) as exc ->
                  let () = 
                    fail_or_continue t.failsafe ( fun _ -> str ) exc () 
                  in
                  GettextStubCompat.setlocale GettextStubCompat.LC_ALL ""
              )
          | None ->
              GettextStubCompat.setlocale GettextStubCompat.LC_ALL ""
      in
      let () = 
        MapCategory.iter ( fun cat locale -> 
          let str : string = 
            GettextStubCompat.setlocale (native_category_of_category cat) locale
          in
          ()
        ) t.categories
      in
      let () = 
        MapTextdomain.iter bind_textdomain_one t.textdomains
      in
      fun printf_format textdomain_opt str_id str_plural_opt cat ->
        let check x = 
          if printf_format then
            match GettextFormat.check_format t.failsafe (Singular(str_id,x)) with
              Singular(_,str) -> str
            | _ -> str_id
          else
            x
        in
        let ncat = 
          native_category_of_category cat
        in
        let textdomain = 
          match textdomain_opt with
            Some textdomain ->
              textdomain
          | None ->
              t.default
        in
        let translation = 
          match str_plural_opt with
              Some(str_plural,n) ->
                GettextStubCompat.dcngettext textdomain str_id str_plural n ncat
            | None ->
                GettextStubCompat.dcgettext textdomain str_id ncat
        in
        check translation

  end
;;

(** Native implementation of gettext. Use the Native module, but use
    informations provided to preload all textdomain translation. The preload 
    is made by trying to translate the string "", which is mandatory in MO file.
    This is not the default behavior of gettext. Use this module if you know
    that it is better to preload all string. Don't use this module if you think
    you will only have a few strings to translate.
  *)
module Preload : GettextRealize.REALIZE_TYPE =
  struct

    let realize t = 
      let t' = Native.realize t
      in
      let () = 
        MapTextdomain.iter ( fun textdomain _ ->
          (* We only load LC_MESSAGES, since it is what is mainly use with
             gettext. Anyway, this is just a local optimization... 
           *)
          let str : string = 
            t' false (Some textdomain) "" None LC_MESSAGES
          in
          ()
        ) t.textdomains
      in
      t'

  end
;;
