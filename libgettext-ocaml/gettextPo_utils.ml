open GettextTypes;;

(* BUG : this exception is defined here, but should be defined in GettextTypes *)
exception PoInconsistentMerge of string * string;;

let empty_po = 
  {
    no_domain = MapString.empty;
    domain    = MapTextdomain.empty;
  }
;;

(* See GettextPo for details concerning merge of the translation *)
let add_po_translation_aux map (location_lst,translation) =
  let is_lst_empty lst = 
    List.for_all ( ( = ) "") lst
  in
  let is_lst_same lst1 lst2 = 
    try
      not (List.exists2 ( fun a b -> a <> b ) lst1 lst2)
    with  Invalid_argument _ ->
      false
  in
  let string_of_list lst = 
    let list_escaped = 
      List.map ( fun s -> Printf.sprintf "%S" s ) lst
    in
    Printf.sprintf "[ %a ]" ( fun () lst -> String.concat ";" lst) lst
  in
  let str_id = 
    match translation with
      Singular(str,_) 
    | Plural(str,_,_) -> str
  in
  try
    let (previous_location_lst,previous_translation) = 
      MapString.find str_id map
    in
    let merged_translation =
      match (previous_translation,translation) with
        Singular(_,str1), Singular(_,str2) when str1 = str2 ->
          Singular(str_id,str1)
      | Singular(_,""), Singular(_,str2) ->
          Singular(str_id,str2)
      | Singular(_,str1), Singular(_,"") ->
          Singular(str_id,str1)
      | Singular(_,str1), Singular(_,str2) ->
          raise (PoInconsistentMerge(str1,str2))
      | Plural(_,str1,lst1), Plural(_,str2,lst2) 
        when str1 = str2 && is_lst_empty lst1 ->
          Plural(str_id,str2,lst2)
      | Plural(_,str1,lst1), Plural(_,str2,lst2) 
        when str1 = str2 && is_lst_empty lst2 ->
          Plural(str_id,str1,lst1)
      | Plural(_,str1,lst1), Plural(_,str2,lst2) 
        when str1 = str2 && is_lst_same lst1 lst2 ->
          Plural(str_id,str1,lst1)
      | Plural(_,str1,lst1), Plural(_,str2,lst2) 
        when str1 = str2 ->
          raise (PoInconsistentMerge(string_of_list lst1,string_of_list lst2))
      | Plural(_,str1,_), Plural(_,str2,_) ->
          raise (PoInconsistentMerge(str1,str2))
      | Singular(_,str), Plural(_,str_plural,lst) 
      | Plural(_,str_plural,lst), Singular(_,str) ->
          (
            match lst with
              "" :: tl ->
                Plural(str_id, str_plural, str :: tl) 
            | [] ->
                Plural(str_id, str_plural, [ str ])
            | _ ->
                raise (PoInconsistentMerge(str, string_of_list lst))
          )
    in
    MapString.add str_id (location_lst @ previous_location_lst, merged_translation) map 
  with Not_found ->
    MapString.add str_id (location_lst,translation) map
;;

let add_po_translation_no_domain po (location_lst,translation) =
  { 
    po with no_domain = 
      add_po_translation_aux po.no_domain (location_lst,translation)
  }
;;

let add_po_translation_domain domain po (location_lst,translation) =
  {
    po with domain =
      let map_domain = 
        try
          MapTextdomain.find domain po.domain
        with Not_found ->
          MapString.empty
      in
      let map_domain = 
        add_po_translation_aux map_domain (location_lst,translation)
      in
      MapTextdomain.add domain map_domain po.domain
  }
;;
