open GettextUtils;;
open GettextTypes;;
open GettextMo;;

open FileUtil;;
open FileUtil.StrUtil;;
open FilePath.DefaultPath;;

exception PoFileInvalid of Lexing.lexbuf * in_channel ;;
exception PoFileInvalidIndex of string * int;;
exception PoFileDoesntExist of string;;
exception PoInconsistentMerge of string * string;;

let string_of_exception exc = 
  match exc with 
    PoFileInvalid (lexbuf,chn) ->
      "Error while processing parsing of PO file : \n"^
      string_of_pos lexbuf
  | PoFileInvalidIndex (id,i) ->
      "Error while processing parsing of PO file, in msgid "
      ^id^", "^(string_of_int i)^" index is out of bound "
  | PoFileDoesntExist fl ->
      "Error while trying to load PO file "^fl^", file doesn't exist"
  | PoInconsistentMerge (str1,str2) ->
      "Error while merging two PO : "^str1^" and "^str2^" cannot be merged"
  | _ ->
      raise exc
;;

(** empty_po : value representing an empty PO *)
let empty_po = 
  GettextPo_utils.empty_po
;;

(** add_po_translation_no_domain po (location_lst,translation) : add a translation 
    to a corpus of already defined translation with no domain defined. If the 
    translation already exist, they are merged concerning location, and 
    follow these rules for the translation itself : 
      - singular and singular : if there is an empty string ( "" ) in one
        of the translation, use the other translation,
      - plural and plural : if there is an empty string list ( [ "" ; "" ] ) in
        one of the translaiton, use the other translation,
      - singular and plural : merge into a plural form.
    There is checks during the merge that can raise PoInconsistentMerge : 
      - for one singular string if the two plural strings differs
      - if there is some elements that differs ( considering the special case of 
        the empty string ) in the translation
*)
let add_po_translation_no_domain po (location_lst,translation) =
  try 
    GettextPo_utils.add_po_translation_no_domain po (location_lst,translation)
  with GettextPo_utils.PoInconsistentMerge(str1,str2) ->
    raise (PoInconsistentMerge(str1,str2))
;;

(** add_po_translation_domain po domain (location_lst,translation) : add a
    translation to the already defined translation with the domain defined. 
    See add_translation_no_domain for details.
*)
let add_po_translation_domain po domain (location_lst,translation) =
  try
    GettextPo_utils.add_po_translation_domain po domain (location_lst,translation)
  with GettextPo_utils.PoInconsistentMerge(str1,str2) ->
    raise (PoInconsistentMerge(str1,str2))
;;

(** merge_po po1 po2 : merge two PO. The rule for merging are the same as
    defined in add_po_translation_no_domain. Can raise PoInconsistentMerge 
*)
let merge_po po1 po2 = 
  (* We take po2 as the initial set, we merge po1 into po2 beginning with
    po1.no_domain and then po1.domain *)
  let merge_no_domain =
    MapString.fold ( 
      fun _ translation po -> 
        add_po_translation_no_domain po translation
    ) po1.no_domain po2
  in
  let merge_one_domain domain map_domain po = 
    MapString.fold ( 
      fun _ translation po ->
        add_po_translation_domain domain po translation
    ) map_domain po
  in
  MapTextdomain.fold merge_one_domain po1.domain merge_no_domain
;;

(** merge_pot po pot : merge a PO with a POT. Only consider strings that
    exists in the pot. Always use location as defined in the POT. If a string 
    is not found, use the translation provided in the POT. If a plural is found
    and a singular should be used, downgrade the plural to singular. If a
    singular is found and a plural should be used, upgrade singular to plural,
    using the strings provided in the POT for ending the translation.
  *)
let merge_pot pot po =
  let order_po_map ?(domain) () = 
    match domain with 
      None ->
        po.no_domain :: ( 
          MapTextdomain.fold ( fun _ x lst -> x :: lst ) 
          po.domain []
        )
    | Some domain ->
        let tl = 
          po.no_domain :: (
            MapTextdomain.fold ( 
              fun key x lst -> 
                if key = domain then 
                  lst 
                else 
                  x :: lst 
            ) po.domain []
          )
        in
        try
          (MapTextdomain.find domain po.domain) :: tl
        with Not_found ->
          tl
  in
  let merge_translation map_lst key (location_pot,translation_pot) =
    let translation_merged = 
      try 
        let (_,translation_po) = 
          let map_po = 
            List.find (MapString.mem key) map_lst
          in
          MapString.find key map_po
        in
        (* Implementation of the rule given above *)
        match (translation_pot,translation_po) with
          Singular(str_id,_), Plural(_, _, str :: _ ) -> 
            Singular(str_id, str)
        | Plural(str_id, str_plural, _ :: tl ), Singular(_, str) ->
            Plural(str_id, str_plural, str :: tl)
        | Plural(str_id, str_plural, []), Singular(_, str) ->
            Plural(str_id, str_plural, str :: [])
        | _, translation ->
            translation
      with Not_found ->
        (* Fallback to the translation provided in the POT *)
        translation_pot
    in
    (location_pot,translation_merged)
  in
  (* We begin with an empty po, and merge everything according to the rule 
     above. *)
  let merge_no_domain = 
    MapString.fold ( 
      fun key (location_pot,translation_pot) po ->
        add_po_translation_no_domain po 
        (merge_translation (order_po_map ()) key (location_pot,translation_pot))
    ) pot.no_domain empty_po
  in
  let merge_one_domain domain map_domain po = 
    MapString.fold ( 
      fun key (location_pot,translation_pot) po ->
        add_po_translation_domain domain po 
        (merge_translation (order_po_map ~domain:domain ()) key (location_pot,translation_pot))
    ) map_domain po
  in
  MapTextdomain.fold merge_one_domain pot.domain merge_no_domain
;;

let input_po chn =
  let lexbuf = Lexing.from_channel chn
  in
  try 
    GettextPo_parser.msgfmt GettextPo_lexer.token lexbuf
  with 
    Parsing.Parse_error 
  | Failure("lexing: empty token") ->
      raise (PoFileInvalid (lexbuf,chn))
  | InvalidIndex(id,i) ->
      raise (PoFileInvalidIndex(id,i))
  | GettextPo_utils.PoInconsistentMerge(str1,str2) ->
      raise (PoInconsistentMerge(str1,str2))
;;

let output_po chn po =
  let fpf x = Printf.fprintf chn x
  in
  let rec output_po_translation_aux _ (location_lst,translation) = 
    let string_translation = 
      match location_lst with
        [] -> "unknown"
      | lst ->
          String.concat " " (
            List.map ( 
              fun (str,line) -> 
                str^":"^(string_of_int line) 
              ) lst
          )
    in
    fpf "#: %s\n" string_translation;
    (
      match translation with
        Singular(id,str) ->
          (
            fpf "msgid %S\n" id;
            fpf "msgstr %S\n" str
          )
      | Plural(id,id_plural,lst) ->
          (
            fpf "msgid %S\n" id;
            fpf "msgid_plural %S\n" id_plural;
            let _ = List.fold_left 
              ( fun i s -> 
                fpf "msgstr[%i] %S\n" i s; 
                i + 1
              ) 0 lst
            in
            ()
          )
    );
    fpf "\n"
  in
  MapString.iter output_po_translation_aux po.no_domain;
  MapTextdomain.iter ( 
    fun domain map ->
        fpf "domain %S\n\n" domain;
        MapString.iter output_po_translation_aux map
  ) po.domain
;; 
