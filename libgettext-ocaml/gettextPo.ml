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
  let po = 
    try 
      GettextPo_parser.msgfmt GettextPo_lexer.token lexbuf
    with 
      Parsing.Parse_error 
    | Failure("lexing: empty token") ->
        raise (PoFileInvalid (lexbuf,chn))
    | InvalidIndex(id,i) ->
        raise (PoFileInvalidIndex(id,i))
  in
  po
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
  let rec string_of_po_entry_aux entry = 
    match entry with
      Singular(id,str) ->
        (
          add_value "msgid"  id;
          add_value "msgstr" str
        )
    | Plural(id,id_plural,lst) ->
        (
          add_value "msgid" id;
          add_value "msgid_plural" id_plural;
          let _ = List.fold_left 
            (fun i s -> add_value ("msgstr["^(string_of_int i)^"]") s; i + 1)
            0 lst
          in
          ()
        )
  in
  let rec string_of_po_aux po = 
    match po with 
      Domain (str,lst) ->
        (
          add_value "domain" str;
          List.iter string_of_po_entry_aux lst
        )
    | NoDomain(lst) ->
        (
          List.iter string_of_po_entry_aux lst
        )
  in
  List.iter string_of_po_aux po;
  Buffer.contents buffer
;; 

let compile_po ?(default_domain = "messages.mo") ?(output_dir = current_dir) po = 
  let merge_domain map domain =
    match domain with
      (* BUG : utiliser add_extension *)
      Domain(str,lst) -> MapString.add (str^".mo") lst map
      (* BUG : probleme de typage *)
    | NoDomain lst -> MapString.add (default_domain^"") lst map
  in
  let merged_domain = List.fold_left merge_domain MapString.empty po
  in
  let output_domain domain lst = 
    let chn = open_out_bin (make_filename [output_dir;domain])
    in
    output_mo chn lst;
    close_out chn
  in
  MapString.iter output_domain merged_domain
;;
