open GettextUtils;;
open GettextTypes;;

open FileUtil;;
open FileUtil.StrUtil;;

exception InvalidPoFile of Lexing.lexbuf * in_channel ;;
exception PoFileDoesntExist of string;;

let string_of_exception exc = 
  match exc with 
    InvalidPoFile (lexbuf,chn) ->
      "Error while processing parsing of PO file : \n"^
      string_of_pos lexbuf
  | _ ->
      raise exc
;;

let input_po chn =
  let lexbuf = Lexing.from_channel chn
  in
  try 
    GettextPo_parser.msgfmt GettextPo_lexer.token lexbuf
  with 
    Parsing.Parse_error 
  | Failure("lexing: empty token") ->
      raise (InvalidPoFile (lexbuf,chn))
;;

let po_of_file fl =
  if test Exists fl then
    let chn = open_in fl
    in
    let po = input_po chn
    in
    close_in chn;
    po
  else
    raise (PoFileDoesntExist fl)
;;
  
let string_of_po po = 
  let buffer = Buffer.create 256
  in
  let add_value key vl = 
    Buffer.add_string buffer key;
    Buffer.add_string buffer " \"";
    Buffer.add_string buffer vl;
    Buffer.add_string buffer "\"\n"
  in
  let rec string_of_po_aux po = 
    match po with 
      Domain (str,lst) ->
        (
          add_value "domain" str;
          List.iter string_of_po_aux lst
        )
    | SingularEntry(strid,str) ->
        (
          add_value "msgid"  strid;
          add_value "msgstr" str
        )
    | PluralEntry(strid,str,lst) ->
        (
          add_value "msgid"  str;
          add_value "msgid_plural" str;
          List.iter 
          (fun (i,s) -> add_value ("msgstr["^(string_of_int i)^"]") s)
          lst
        )
  in
  List.iter string_of_po_aux po;
  Buffer.contents buffer
;; 


