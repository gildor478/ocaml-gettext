(** Implements functions helping check that two strings are equivalent, regarding printf use *)

(** check_format failsafe translation : returns a translation structure
    if all the string contained in the translation are equivalent of
    str_id, regarding printf format. If not, replace each string which conflict
    by str_id, in the result *)
let check_format failsafe translation = 
  let format_lst_of_string str =
    let lexbuf = Lexing.from_string str
    in
    GettextFormat_parser.main GettextFormat_lexer.token lexbuf 
  in
  let check_format_lst_str lst1 lst2 = 
    let check_format_aux fc1 fc2 = 
      if fc1 = fc2 then
        ()
      else
        raise FormatInconsistent(fc1,fc2) 
    in
    List.iter2 check_format_aux lst1 lst2
  in
  match translation with
    Singular (str_id,str) ->
  | Plural (str
