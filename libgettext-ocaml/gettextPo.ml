open GettextUtils;;
open GettextTypes;;
open GettextMo;;

open FileUtil;;
open FileUtil.StrUtil;;
open FilePath.DefaultPath;;

exception PoFileInvalid of Lexing.lexbuf * in_channel ;;
exception PoFileInvalidIndex of string * int;;
exception PoFileDoesntExist of string;;

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
      raise (PoFileInvalid (lexbuf,chn))
  | InvalidIndex(id,i) ->
      raise (PoFileInvalidIndex(id,i))
;;

let output_po chn po =
  let fpf x = Printf.fprintf chn x
  in
  let rec output_po_entry_aux entry = 
    fpf "#: (this should be the location)\n";
    (
      match entry with
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
  List.iter output_po_entry_aux po.no_domain;
  List.iter ( 
    fun (domain,lst) ->
        fpf "domain %S\n\n" domain;
        List.iter output_po_entry_aux lst
  ) po.domain
;; 
