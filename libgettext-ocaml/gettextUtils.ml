open Lexing;;

let string_of_pos lexbuf = 
  let char_pos = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol
  in
  let line_pos = lexbuf.lex_curr_p.pos_lnum
  in
  "line "^(string_of_int line_pos)
  ^" character "
  ^(string_of_int char_pos)
;;
