(** Signature of module for domain management *)

open FilePath;;
open FilePath.DefaultPath;;
open FileUtil;;
open FileUtil.StrUtil;;
open GettextTypes;;
open GettextUtils;;
open GettextCategory;;

(* BUG : a mettre à jour *)
(** compute_path textdomain category t : return the path to the 
    mo file corresponding to textdomain and category. Language is 
    guessed from category binding. If the textdomain is not found,
    it tries to use the build default to find the file. The file 
    returned exists and is readable. If such a file doesn't exists 
    an exception DomainFileDoesntExist is thrown. If the function is 
    unable to guess the current language an exception 
    DomainLanguageNotSet is thrown.
*)

let make_filename dir language category textdomain = 
  (* http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC148 
    dir_name/locale/LC_category/domain_name.mo *)
  make_filename [
    (* BUG : should use add_extension *)
    dir; language; string_of_category category; textdomain ^ ".mo" 
  ]
;;


let find t languages category textdomain = 
  let search_path =
    (
      try 
        match MapTextdomain.find textdomain t.textdomains with 
          (_,Some dir) -> [dir]
        | (_,None) -> []
      with Not_found ->
        []
      ) @ t.path
  in
  let ctest = test (And(Exists,Is_readable))
  in
  let rec find_mo_file_aux dir languages =
    match languages with 
     language :: tl ->
       let current_filename = make_filename dir language category textdomain
       in
       if ctest current_filename then
         current_filename
       else
         find_mo_file_aux dir tl
    |  [] ->
        raise Not_found
  in
  let rec find_mo_file path languages =
    match path with
      dir :: tl ->
        (
          try
            find_mo_file_aux dir languages
          with Not_found ->
            find_mo_file tl languages
        )
    | [] ->
        raise Not_found 
  in
  try
    find_mo_file search_path languages
  with Not_found ->
    raise (DomainFileDoesntExist(
        List.flatten (
          List.map ( 
            fun dir ->
              List.map (
                fun language ->
                  make_filename dir language category textdomain
            ) languages
          ) search_path
        )
      )
    )
;;
