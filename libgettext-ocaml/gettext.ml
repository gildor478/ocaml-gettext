open GettextTypes;;
open GettextCompat;;

(* Function the main global variable of gettext with/without thread *)

type global_type = {
  t          : t option;
  realize    : realize; 
  t'         : t' option;
}
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
  realize    = (fun t textdomain str str_plural category -> str);
  t'         = None;
}
;;

let get_global_t () = 
  let t = 
    global_lock ();
    !global.t
  in
  global_unlock ();
  t
;;

let set_global_t t = 
  !critical_section (
    fun () ->
      global := { 
        t       = Some t;
        realize = !global.realize;
        t'      = None;
      }
  )
;;

let get_global_realize () =
  !critical_section (
    fun () ->
      !global.realize
  )
;;

let set_global_realize realize = 
  !critical_section (
    fun () ->
      global := { !global with realize = realize }
  )
;;

let get_global_t' () =
  !critical_section (
    fun () ->
      match !global.t' with
        None ->
          (* Try to build t' out of the other value provided *)
          let t = 
            match !global.t with
              Some t -> t
            | None -> raise GettextUnitialized
          in
          let realize = 
            !global.realize
          in
          !global := { !global with t' = Some t' };
          t'
      | Some t' ->
          t'
  )
;;
          

(* High level functions *)

module Library =
  functor ( Init : Init ) ->
  struct
    let init = (Init.textdomain, Init.codeset, Init.dir) :: Init.dependencies

    let s_   = dgettext (get_global_t' ()) Init.textdomain 
    let f_   = fdgettext (get_global_t' ()) Init.textdomain
    let sn_  = dngettext (get_global_t' ()) Init.textdomain
    let fn_  = fdngettext (get_global_t' ()) Init.textdomain
  end
;;

module Program = 
  functor ( Init : Init ) ->
  functor ( Realize : realize ) ->
  struct
    let textdomain = Init.textdomain

    let init = [  
      (
        "--gettext-failsafe",
        ( Arg.Symbol 
          (
            ["ignore"; "inform-stderr"; "raise-exception"],
            ( fun x ->
                match x with
                  "ignore"          -> set_global { (get_global ()) with failsafe = Ignore }
                | "inform-stderr"   -> set_global { (get_global ()) with failsafe = InformStderr }
                | "raise-exception" -> set_global { (get_global ()) with failsafe = RaiseException }
            )
          )
        ),
        "Choose how to handle failure in gettext ( ignore, stderr, exception )" 
      );
      (
        "--gettext-disable",
        ( Arg.Unit 
          ( fun () -> set_global { (get_global ()) with realize = XXX } 
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
          ( fun s -> set_global { (get_global ()) with dir = s }
          )
        ),
        "Set the default dir to search gettext files"
      );
      (
        "--gettext-language",
        ( Arg.String
          ( fun s -> set_global { (get_global ()) with language = s }
          )
        ),
        "Set the default language for gettext"
      );
      (
        "--gettext-codeset",
        ( Arg.String
          ( fun s -> set_global { (get_global ()) with codeset = s }
          )
        ),
        "Set the default codeset for outputting string with gettext"
      );
      ]
      
    let s_   = dgettext (get_global_t' ()) textdomain 
    let f_   = fdgettext (get_global_t' ()) textdomain
    let sn_  = dngettext (get_global_t' ()) textdomain
    let fn_  = fdngettext (get_global_t' ()) textdomain

  end
;;
