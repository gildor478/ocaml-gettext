open Lexing;;

(* For communication between gettextPo_parser / gettextPo *)
exception InvalidIndex of string * int;;

(* BUG : plus utile *)
module SetInt = Set.Make (struct
  type t      = int
  let compare = compare
end)
;;
  
module MapString = Map.Make (struct
  type t      = string
  let compare = compare
end)
;;
  
let string_of_pos lexbuf = 
  let char_pos = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol
  in
  let line_pos = lexbuf.lex_curr_p.pos_lnum
  in
  "line "^(string_of_int line_pos)
  ^" character "
  ^(string_of_int char_pos)
;;

let split_plural str =
  let rec split_plural_one start =
    try 
      let next_sep = String.index_from str start '\000' 
      in
      let new_plural = String.sub str start (next_sep - start)
      in
      if (next_sep + 1) >= String.length str then
        [new_plural]
      else
        new_plural :: ( split_plural_one (next_sep + 1) )
    with Not_found ->
      [str]
  in
  split_plural_one 0
;;
