open Lexing;;
open GettextTypes;;

(* For communication between gettextPo_parser / gettextPo *)
exception InvalidIndex of string * int;;

 
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
    let next_sep = 
      try
        String.index_from str start '\000' 
      with Not_found ->
        String.length str
    in
    let new_plural = String.sub str start (next_sep - start)
    in
    if (next_sep + 1) >= String.length str then
      [new_plural]
    else
      new_plural :: ( split_plural_one (next_sep + 1) )
  in
  split_plural_one 0
;;

let fail_or_continue failsafe exc_printer exc cont_value =
  match failsafe with
    Ignore ->
      cont_value
  | InformStderr ->
      (
        prerr_string (exc_printer exc);
        prerr_newline ();
        cont_value
      )
  | RaiseException ->
      raise exc
;;


