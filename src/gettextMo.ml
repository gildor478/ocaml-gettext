open GettextTypes;;
open GettextMo_int32;;
 
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
          raise Bad_mo_file 
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

let input_mo_translation chn mo_header =
  let input_strings_list () = 
    let input_one_string (length,offset) = 
      let str = String.make (Int32.to_int length) 'X'
      in
      seek_in chn (Int32.to_int offset);
      really_input chn str 0 (Int32.to_int length);
      str
    in
    List.map input_one_string
      (
        input_int32_pair_table
        chn 
        mo_header.endianess 
    (Int32.to_int mo_header.number_of_strings) 
      )
   in
   let list_strings = 
     seek_in chn (Int32.to_int mo_header.offset_table_strings);
     input_strings_list ()
   in
   let list_translations = 
     seek_in chn (Int32.to_int mo_header.offset_table_translation);
     input_strings_list ()
   in
   let translations = 
     Hashtbl.create (Int32.to_int mo_header.number_of_strings)
   in
   let empty_translation = 
     List.iter2 ( fun s t -> Hashtbl.add translations s t ) list_strings
     list_translations;
     try
       let res = Hashtbl.find translations ""
       in
       Hashtbl.remove translations "";
       res
     with Not_found ->
       ""
   in
   let string_of_pos lexbuf = 
     "line "^(string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)
     ^" character "
     ^(string_of_int (lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol))
   in
   let field_value = 
     let lexbuf = Lexing.from_string empty_translation
     in
     try
       GettextMo_parser.main GettextMo_lexer.token_field_name lexbuf
     with 
       Parsing.Parse_error 
     | Failure("lexing: empty token") ->
       (
         prerr_string "Error while processing parsing of options : ";
         prerr_newline ();
         prerr_string (string_of_pos lexbuf);
         prerr_newline ();
         prerr_string empty_translation;
         prerr_newline ();
         []
       )
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
         (
           prerr_string "Error while processing parsing of plural : ";
           prerr_newline ();
           prerr_string (string_of_pos lexbuf);
           prerr_newline ();
           prerr_string field_plural_forms;
           prerr_newline ();
           germanic_plural
         ) 
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
         (
           prerr_string "Error while processing parsing of content-type : ";
           prerr_newline ();
           prerr_string (string_of_pos lexbuf);
           prerr_newline ();
           prerr_string field_content_type;
           prerr_newline ();
           gettext_content
         )
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
     translation_from_string   = translations;
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
      Some s ->
        s
    | None ->
        ""
  in
  p buff "Translations              : \n";
  if omit_translation then
    p buff "omited\n"
  else
    Hashtbl.iter ( fun s t -> p buff "\"%s\" -> \"%s\"\n" s t ) mo_translation.translation_from_string
  ;
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
