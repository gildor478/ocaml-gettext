open GettextTypes;;
open GettextCompat;;
open Lexing;;

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

(* i18n/l10n of gettext it self *)
module GettextGettext = Library(struct
  let textdomain   = "ocaml-gettext"
  let codeset      = None
  let dir          = None
  let dependencies = [] 
  (* Off course, we don't depend on anything because  
     we are the root of translation *)
  end)
;;

(* Exception *)

let string_of_exception exc = 
  (* It is important to keep the name f_ and s_, in order to allow ocaml-gettext 
     program to extract the string *)
  let f_ x = 
    GettextGettext.f_ x
  in
  let s_ x =
    GettextGettext.s_ x
  in
  let spf x = 
    Printf.sprintf x
  in
  let string_of_list lst = 
    "[ "^(String.concat "; " (List.map (fun str -> spf "%S" str) lst))^" ]"
  in
  let string_of_pos lexbuf = 
    let char_pos = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol
    in
    let line_pos = lexbuf.lex_curr_p.pos_lnum
    in
    spf (f_ "line %d character %d")
    line_pos char_pos
  in
  match exc with
    ProblemReadingFile(fln,error) ->
      spf (f_ "Problem reading file %s : %s") fln error
  | ExtractionFailed(fln,cmd,status) ->
      spf (f_ "Problem while extracting %s : command %S exits with code %d")
      fln cmd status
  | ExtractionInterrupted(fln,cmd,signal) ->
      spf (f_ "Problem while extracting %s : command %S killed by signal %d")
      fln cmd signal
  | DomainFileDoesntExist(lst) ->
      spf (f_ "Cannot find an approriate gettext compiled file ( %s )")
      (string_of_list lst)
  | GettextUninitialized -> 
      (s_ "Gettext library is not initialized")
  | InvalidOptions (lexbuf,text) ->
      spf (f_ "Error while processing parsing of options at %s : %S")
      (string_of_pos lexbuf)
      text
  | InvalidPlurals (lexbuf,text) ->
      spf (f_ "Error while processing parsing of plural at %s : %S")
      (string_of_pos lexbuf)
      text
  | InvalidContentType (lexbuf,text) ->
      spf (f_ "Error while processing parsing of content-type at %s : %S")
      (string_of_pos lexbuf)
      text
  | InvalidMoFile ->
      (s_ "MO file provided is not encoded following gettext convention")
  | InvalidTranslationSingular (str,x) ->
      spf (f_ "Trying to fetch the plural form %d of a singular form %S")
      x str
  | InvalidTranslationPlural (lst,x) ->
      spf (f_ "Trying to fetch the plural form %d of plural form %s")
      x (string_of_list lst)
  | Junk (id,lst) ->
      spf (f_ "Junk at the end of the plural form id %S: %s")
      id (string_of_list lst)
  | EmptyEntry ->
      (s_ "An empty entry has been encounter")
  | InvalidMoHeaderNegativeStrings ->
      (s_ "Number of strings is negative")
  | InvalidMoHeaderTableStringOutOfBound((b1,e1),(b2,e2)) ->
      spf (f_ "Offset of string table is out of bound ([%ld,%ld] should be in [%ld,%ld])")
      b1 e1 b2 e2
  | InvalidMoHeaderTableTranslationOutOfBound((b1,e1),(b2,e2)) ->
      spf (f_ "Offset of translation table is out of bound ([%ld,%ld] should be in [%ld,%dl])")
      b1 e1 b2 e2
  | InvalidMoHeaderTableTranslationStringOverlap((b1,e1),(b2,e2)) ->
      spf (f_ "Translation table and string table overlap ([%ld,%ld] and [%ld,%ld] have a non empty intersection)")
      b1 e1 b2 e2
  | InvalidMoStringOutOfBound(max,cur) ->
      spf (f_ "Out of bound access when trying to find a string (%d < %d)")
      max cur
  | InvalidMoTranslationOutOfBound(max,cur) ->
      spf (f_ "Out of bound access when trying to find a translation (%d < %d)")
      max cur
  | CannotOpenMoFile fln ->
      spf (f_ "Could not open file %s")
      fln
  | PoFileInvalid (s,lexbuf,chn) ->
      spf (f_ "Error while processing parsing of PO file: %S at %s")
      s (string_of_pos lexbuf)
  | PoFileInvalidIndex (id,i) ->
      spf (f_ "Error while processing parsing of PO file, in msgid %S, %d index is out of bound")
      id i
  | PoFileDoesntExist fl ->
      spf (f_ "Error while trying to load PO file %s, file doesn't exist") 
      fl
  | PoInconsistentMerge (str1,str2) ->
      spf (f_ "Error while merging two PO : %S and %S cannot be merged")
      str1 str2
  | GettextTranslateStringNotFound str ->
      spf (f_ "Cannot find string %S")
      str
  | _ ->
      Printexc.to_string exc
;;


module Program = 
  functor ( Init : InitProgram ) ->
  struct
    let textdomain = Init.textdomain

    let init = 
      (* Initialization from all the known textdomain/codeset/dir provided 
         by library linked with the program *)
      (* It is important to keep f_ and s_, for the same reason as in
         string_of_exception *)
      let f_ x = 
        GettextGettext.f_ x
      in
      let s_ x =
        GettextGettext.s_ x
      in
      let spf x = 
        Printf.sprintf x
      in
      let () = 
        set_global_t (create textdomain)
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
                  | "inform-stderr"   -> set_global_t { (get_global_t ()) with failsafe = InformStderr string_of_exception }
                  | "raise-exception" -> set_global_t { (get_global_t ()) with failsafe = RaiseException }
                  | _                 -> ()
              )
            )
          ),
          spf (f_ "Choose how to handle failure in gettext ( ignore, stderr, exception ). Default: %s")
          (
            ( function 
                  Ignore -> "ignore" 
                | InformStderr _ -> "stderr" 
                | RaiseException -> "exception"    
            ) (get_global_t ()).failsafe
          )
        );
        (
          "--gettext-disable",
          ( Arg.Unit 
            ( fun () -> set_global_realize dummy_realize 
            )
          ),
          (s_ "Disable the translation perform by gettext")
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
          "Set a dir to search gettext files for the specified domain. Default: "
          ...
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
