open GettextTypes;;
open GettextMo_int32;;
 
exception InvalidOptions of Lexing.lexbuf * string;;
exception InvalidPlurals of Lexing.lexbuf * string;;
exception InvalidContentType of Lexing.lexbuf * string;;
exception InvalidTranslationSingular of string * int;;
exception InvalidTranslationPlural of (string list) * int;;
exception InvalidMoFile;;

let string_of_exception exc =   
  let string_of_pos lexbuf = 
    "line "^(string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)
    ^" character "
    ^(string_of_int (lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol))
  in
  match exc with
    InvalidOptions (lexbuf,text) ->
      "Error while processing parsing of options : \n"
      ^(string_of_pos lexbuf)^"\n"
      ^text
  | InvalidPlurals (lexbuf,text) ->
      "Error while processing parsing of plural : \n"
      ^(string_of_pos lexbuf)^"\n"
      ^text
  | InvalidContentType (lexbuf,text) ->
      "Error while processing parsing of content-type : \n"
      ^(string_of_pos lexbuf)^"\n"
      ^text
  | InvalidMoFile ->
      "MO file provided is not encoded following gettext convention"
  | InvalidTranslationSingular (str,x) ->
      "Trying to fetch the plural form "
      ^(string_of_int x)
      ^" of a singular form \""
      ^str^"\""
  | InvalidTranslationPlural (lst,x) ->
      "Trying to fetch the plural form "
      ^(string_of_int x)
      ^" of plural form [ \""
      ^(String.concat "\"; \"" lst)
      ^"\" ]"
  | _ ->
      ""
;;

let fail_or_continue failsafe exc cont_value =
  match failsafe with
    Ignore ->
      cont_value
  | InformStderr ->
      (
        prerr_string (string_of_exception exc);
        prerr_newline ();
        cont_value
      )
  | RaiseException ->
      raise exc
;;

let input_mo_header chn = 
  let endianess = 
    let magic_number = seek_in chn 0; input_int32 chn ArchEndian
    in
    match Int32.to_int magic_number with
      0x950412de ->
        ArchEndian
    | 0xde120495 ->
        NotArchEndian
    | _ ->
          raise InvalidMoFile
  in
  let seek_and_input x = seek_in chn x; input_int32 chn endianess
  in
  {
    endianess                = endianess;
    file_format_revision     = seek_and_input  4;
    number_of_strings        = seek_and_input  8;
    offset_table_strings     = seek_and_input 12;
    offset_table_translation = seek_and_input 16;
    size_of_hashing_table    = seek_and_input 20;
    offset_of_hashing_table  = seek_and_input 24;
  }
;;

let string_of_mo_header mo_header = 
  let buff = Buffer.create 256
  in
  Printf.bprintf buff "File format revision                     : %ld\n" mo_header.file_format_revision;
  Printf.bprintf buff "Number of string                         : %ld\n" mo_header.number_of_strings;
  Printf.bprintf buff "Offset of table with original strings    : %lx\n" mo_header.offset_table_strings;
  Printf.bprintf buff "Offset of table with translation strings : %lx\n" mo_header.offset_table_translation;
  Printf.bprintf buff "Size of hashing table                    : %lx\n" mo_header.size_of_hashing_table;
  Printf.bprintf buff "Offset of hashing table                  : %lx\n" mo_header.offset_of_hashing_table;
  Buffer.contents buff
;;

let input_mo_untranslated ?(failsafe = Ignore) chn mo_header number = 
  let offset_pair = 
    (Int32.to_int mo_header.offset_table_strings) + number * 8
  in
  seek_in chn offset_pair;
  input_int32_pair_string chn mo_header.endianess
;;

let input_mo_translated ?(failsafe = Ignore) chn mo_header number = 
  let offset_pair = 
    (Int32.to_int mo_header.offset_table_translation) + number * 8
  in
  let str = 
    seek_in chn offset_pair;
    input_int32_pair_string chn mo_header.endianess
  in
  if String.contains str '\000' then
    let rec split_plural start =
      try 
        let next_sep = String.index_from str start '\000' 
        in
        let new_plural = String.sub str start (next_sep - start)
        in
        if (next_sep + 1) >= String.length str then
          [new_plural]
        else
          new_plural :: ( split_plural (next_sep + 1) )
      with Not_found ->
        [str]
    in
    Plural (split_plural 0)
  else
    Singular str
;;

let get_translated_value ?(failsafe = Ignore) translation plural_number =
  match (translation, plural_number) with
    (Singular str, 0) ->
      str
  | (Singular str, x) ->
      fail_or_continue 
      failsafe
      (InvalidTranslationSingular(str,x))
      str
  | (Plural lst, x) when x < List.length lst ->
      List.nth lst x 
  | (Plural lst, x) ->
      fail_or_continue 
      failsafe
      (InvalidTranslationPlural(lst,x))
      List.nth lst 0
;;

let input_mo_informations ?(failsafe = Ignore) chn mo_header =
  (* La position de "" est forcément 0 *)
  let empty_translation = 
    get_translated_value ~failsafe 
    (input_mo_translated chn mo_header 0)
    0
  in
  let field_value = 
    let lexbuf = Lexing.from_string empty_translation
    in
    try
      GettextMo_parser.main GettextMo_lexer.token_field_name lexbuf
    with 
      Parsing.Parse_error 
    | Failure("lexing: empty token") ->
        fail_or_continue 
        failsafe 
        (InvalidOptions (lexbuf,empty_translation)) 
        []
  in
  let (nplurals,fun_plural_forms) = 
    let germanic_plural = 
      (* The germanic default *)
      (2,fun n -> if n = 1 then 1 else 0)
    in
    try 
      let field_plural_forms = List.assoc "Plural-Forms" field_value
      in
      let lexbuf = Lexing.from_string field_plural_forms
      in
      try
        GettextMo_parser.plural_forms
        GettextMo_lexer.token_field_plural_value lexbuf 
      with 
        Parsing.Parse_error 
      | Failure("lexing: empty token") ->
          fail_or_continue 
          failsafe 
          (InvalidPlurals(lexbuf,field_plural_forms))
          germanic_plural
    with Not_found ->
      germanic_plural 
  in
  let (content_type, content_type_charset) = 
    let gettext_content = ("text/plain", "UTF-8")
    in
    try 
      let field_content_type = List.assoc "Content-Type" field_value
      in
      let lexbuf = Lexing.from_string field_content_type
      in
      try
        GettextMo_parser.content_type 
        GettextMo_lexer.token_field_content_type lexbuf
      with      
        Parsing.Parse_error 
      | Failure("lexing: empty token") ->
          fail_or_continue
          failsafe
          (InvalidContentType(lexbuf,field_content_type))
          gettext_content
    with Not_found ->
      gettext_content 
  in
  let extract_field_string name = 
    try 
      Some (List.assoc name field_value)
    with Not_found ->
      None
  in
   {
     project_id_version        = extract_field_string "Project-Id-Version";
     report_msgid_bugs_to      = extract_field_string "Report-Msgid-Bugs-To";
     pot_creation_date         = extract_field_string "POT-Creation-Date"; 
     po_revision_date          = extract_field_string "PO-Revision-Date";
     last_translator           = extract_field_string "Last-Translator";
     language_tream            = extract_field_string "Language-Team";
     mime_version              = extract_field_string "MIME-Version";
     content_type              = extract_field_string "Content-Type";
     content_transfer_encoding = extract_field_string "Content-Transfer-Encoding";
     plural_forms              = extract_field_string "Plural-Forms";
     content_type_charset      = content_type_charset;
     nplurals                  = nplurals;
     fun_plural_forms          = fun_plural_forms;
   }
;;

let string_of_mo_translation ?(omit_translation=true) ?(compute_plurals=(0,3)) mo_translation = 
  let buff = Buffer.create 1024
  in
  let p = Printf.bprintf 
  in
  let extract_string x = 
    match x with
      Some s -> s
    | None -> ""
  in
  p buff "Project-Id-Version        : %s\n" (extract_string mo_translation.project_id_version);
  p buff "Report-Msgid-Bugs-To      : %s\n" (extract_string mo_translation.report_msgid_bugs_to);
  p buff "POT-Creation-Date         : %s\n" (extract_string mo_translation.pot_creation_date); 
  p buff "PO-Revision-Date          : %s\n" (extract_string mo_translation.po_revision_date);
  p buff "Last-Translator           : %s\n" (extract_string mo_translation.last_translator);
  p buff "Language-Team             : %s\n" (extract_string mo_translation.language_tream);
  p buff "MIME-Version              : %s\n" (extract_string mo_translation.mime_version);
  p buff "Content-Type              : %s\n" (extract_string mo_translation.content_type);
  p buff "Plurals-Forms             : %s\n" (extract_string mo_translation.plural_forms);
  p buff "Content-Transfer-Encoding : %s\n" (extract_string mo_translation.content_transfer_encoding);
  p buff "Content-Type-Charsert     : %s\n" mo_translation.content_type_charset;
  p buff "NPlurals                  : %d\n" mo_translation.nplurals;
  p buff "Fun plural                : \n";
  let (a,b) = compute_plurals
  in
  for i = a to b do 
    p buff "%d -> %d\n" i (mo_translation.fun_plural_forms i);
  done;
  Buffer.contents buff
;;
