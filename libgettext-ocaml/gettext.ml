open GettextTypes;;
open GettextCompat;;

(* Exceptions *)

exception GettextUninitialized;;

let string_of_exception exc = 
  match exc with 
    GettextUninitialized -> "Gettext library is not initialized"
  | _ -> ""
;;

(* Function the main global variable of gettext with/without thread *)

type global_type = {
  t          : t option;
  realize    : realize; 
  t'         : t' option;
}
;;

(* Default value *)

let dummy_realize =
  (fun t printf_format textdomain str str_plural category -> str)
;;

let default_realize = 
  dummy_realize
;;

(* Referenced function used to manage access to global variable, 
*  in other word to be fullfiled with mutex locking/unlocking if 
*  needed
*)

let global_lock = ref ( fun () -> () )
;;

let global_unlock = ref ( fun () -> () )
;;

let global = ref {
  t          = None;
  realize    = default_realize;
  t'         = None;
}
;;

let get_global_t () = 
  let t = 
    !global_lock ();
    !global.t
  in
  !global_unlock ();
  match t with
    Some t -> t
  | None -> raise GettextUninitialized
;;

let set_global_t t = 
  let () = 
    !global_lock ();
    global := { 
      t       = Some t;
      realize = !global.realize;
      t'      = None;
    }
  in
  !global_unlock ()
;;

let get_global_realize () =
  let t = 
    !global_lock ();
    !global.realize
  in
  !global_unlock ();
  t
;;

let set_global_realize realize = 
  let () = 
    !global_lock (); 
    global := { !global with realize = realize }
  in
  !global_unlock ()
;;

let get_global_t' () =
  let t' =
    !global_lock ();
    match !global.t' with
      None ->
        (* Try to build t' out of the other value provided *)
        let t = 
          match !global.t with
            Some t -> t
          | None -> raise GettextUninitialized
        in
        let t' = 
          !global.realize t
        in
        global := { !global with t' = Some t' };
        t'
    | Some t' ->
        t'
  in
  !global_unlock ();
  t'
;;
          

(* High level functions *)

module Library =
  functor ( Init : Init ) ->
  struct
    let init = (Init.textdomain, Init.codeset, Init.dir) :: Init.dependencies

    let s_  str = dgettext (get_global_t' ()) Init.textdomain str
    let f_  str = fdgettext (get_global_t' ()) Init.textdomain str
    let sn_ str = dngettext (get_global_t' ()) Init.textdomain str
    let fn_ str = fdngettext (get_global_t' ()) Init.textdomain str
  end
;;

module Program = 
  functor ( Init : InitProgram ) ->
  struct
    let textdomain = Init.textdomain

    let init = 
      (* Initialization from all the known textdomain/codeset/dir provided 
         by library linked with the program *)
      (*let t = 
        create*)
      let current_textdomain = ref textdomain
      in      
      [  
          (
          "--gettext-failsafe",
          ( Arg.Symbol 
            (
              ["ignore"; "inform-stderr"; "raise-exception"],
              ( fun x ->
                  match x with
                    "ignore"          -> set_global_t { (get_global_t ()) with failsafe = Ignore }
                  | "inform-stderr"   -> set_global_t { (get_global_t ()) with failsafe = InformStderr }
                  | "raise-exception" -> set_global_t { (get_global_t ()) with failsafe = RaiseException }
                  | _                 -> ()
              )
            )
          ),
          "Choose how to handle failure in gettext ( ignore, stderr, exception )" 
        );
        (
          "--gettext-disable",
          ( Arg.Unit 
            ( fun () -> set_global_realize dummy_realize 
            )
          ),
          "Disable the translation perform by gettext"
        );
        (
          "--gettext-domain-dir",
          ( Arg.Tuple 
            [
              Arg.String ( fun textdomain -> current_textdomain := textdomain );
              Arg.String ( fun dir -> 
                set_global_t 
                (bindtextdomain !current_textdomain dir (get_global_t ()))
              );
            ]
          ),
          "Set a dir to search gettext files for the specified domain"
        );
        (
          "--gettext-dir",
          ( Arg.String
            ( fun s -> set_global_t { 
                (get_global_t ()) with 
                path = s :: (get_global_t ()).path 
              }
            )
          ),
          "Add a search dir for gettext files"
        );
        (
          "--gettext-language",
          ( Arg.String
            ( fun s -> set_global_t { (get_global_t ()) with language = Some s }
            )
          ),
          "Set the default language for gettext"
        );
        (
          "--gettext-codeset",
          ( Arg.String
            ( fun s -> set_global_t { (get_global_t ()) with codeset = s }
            )
          ),
          "Set the default codeset for outputting string with gettext"
        );
      ], GettextConfig.copyright
      
    let s_  str = dgettext (get_global_t' ()) textdomain str
    let f_  str = fdgettext (get_global_t' ()) textdomain str
    let sn_ str = dngettext (get_global_t' ()) textdomain str
    let fn_ str = fdngettext (get_global_t' ()) textdomain str

  end
;;
