(** Implements functions helping check that two strings are equivalent, regarding printf use *)

module SetTupleString = Set.Make(struct
  type t = string * string
  let compare (s11,s12) (s21,s22) = 
    match compare s11 s21 with
      0 ->
        compare s12 s22
    | x ->
        x
end)
;;

let set_congruence =
  let  lst_congruence = [
      [ "d"; "i"; "n"; "N" ];
      [ "u"; "x"; "X" ];
      [ "o" ];
      [ "s" ];
      [ "S" ];
      [ "c" ];
      [ "C" ];
      [ "f"; "F"; "e"; "E"; "g"; "G" ];
      [ "B"; "b" ];
      [ "ld"; "li" ];
      [ "lu" ];
      [ "lx"; "lX" ];
      [ "lo" ];
      [ "nd"; "ni" ];
      [ "nu" ];
      [ "nx"; "nX" ];
      [ "no" ];
      [ "Ld"; "Li" ];
      [ "Lu" ];
      [ "Lx"; "LX" ];
      [ "Lo" ];
      [ "a" ];
      [ "t" ];
      [ "%" ]
    ]
  in
  let rec add_tuple t lst =
    let add_tuple_string_string elem1 t elem2 = 
      print_endline ("("^elem1^", "^elem2^")");
      SetTupleString.add (elem1,elem2) t
    in
    let add_tuple_string_lst elem1 lst t =
      List.fold_left (add_tuple_string_string elem1) t lst
    in
    match lst with
      hd :: tl ->
        add_tuple (
          add_tuple_string_lst hd lst t
        ) tl
    | [] ->
        t
  in
  List.fold_left add_tuple SetTupleString.empty lst_congruence
;;
 

let split_format str = 
  let rec split_format_aux n = 
    let index = 
      String.index_from str n '%'
    in
  


(** check_format str1 str2 : returns str2 if str2 is equivalent of str1. If not,
    returns str1. Equivalence is checked regarding printf format *)
(*let check_format str1 str2 = 
;;*)
