(** Signature of module for domain management *)

open FilePath;;
open FilePath.DefaultPath;;
open FileUtil;;
open FileUtil.StrUtil;;
open GettextTypes;;
open GettextUtils;;
open GettextCategory;;

exception DomainFileDoesntExist of string * string;; 

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

let compute_path t lst_locale category textdomain = 
  let search_path =
    (
      try 
        match MapTextdomain.find textdomain t.textdomains with 
          (_,Some dir) -> [dir]
        | (_,None) -> []
      with Not_found ->
        []
      ) @ t.dirs
  in
  (* http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC148 
    dir_name/locale/LC_category/domain_name.mo *)
  let make_path dir language = 
    make_filename [
      (* BUG : should use add_extension *)
      dir; language; string_of_category category; textdomain ^ ".mo" 
    ]
  in
  let ctest = test (And(Exists,Is_readable))
  in
  let rec find_mo_file_aux path language =
    match language with 
     lang :: tl ->
       let current_path = make_path path lang
       in
       if ctest current_path then
         current_path
       else
         find_mo_file_aux path tl
    |  [] ->
        raise Not_found
  in
  let rec find_mo_file all_language all_path =
    match search_path with
      path :: tl ->
        (
          try
            find_mo_file_aux path all_language
          with Not_found ->
            find_mo_file all_language tl
        )
    | [] ->
        raise Not_found 
  in
  try
    find_mo_file lst_locale search_path
  with Not_found ->
    raise (DomainFileDoesntExist(textdomain,string_of_category category))
;;
