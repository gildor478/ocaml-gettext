open GettextUtils;;
open GettextTypes;;
open GettextMo_int32;;
 
exception InvalidOptions of Lexing.lexbuf * string;;
exception InvalidPlurals of Lexing.lexbuf * string;;
exception InvalidContentType of Lexing.lexbuf * string;;
exception InvalidTranslationSingular of string * int;;
exception InvalidTranslationPlural of (string list) * int;;
exception Junk of string * string list;;
exception EmptyEntry;;
exception InvalidMoFile;;

let string_of_exception exc =   
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
  | Junk (id,lst) ->
      "Junk at the end of the plural form id "
      ^id^" : [ \""
      ^(String.concat "\"; \"" lst)
      ^"\" ]"
  | EmptyEntry ->
      "An empty entry has been encounter"
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

let output_mo_header chn hdr = 
  let output = output_int32 chn hdr.endianess
  in
  output (Int32.of_int 0x950412de); (* magic_number *)
  output hdr.file_format_revision; 
  output hdr.number_of_strings;
  output hdr.offset_table_strings;
  output hdr.offset_table_translation;
  output hdr.size_of_hashing_table;
  output hdr.offset_of_hashing_table
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
  let str = 
    seek_in chn offset_pair;
    input_int32_pair_string chn mo_header.endianess
  in
  split_plural str
;;

let input_mo_translated ?(failsafe = Ignore) chn mo_header number = 
  let offset_pair = 
    (Int32.to_int mo_header.offset_table_translation) + number * 8
  in
  let str = 
    seek_in chn offset_pair;
    input_int32_pair_string chn mo_header.endianess
  in
  split_plural str 
;;

let input_mo_translation ?(failsafe = Ignore) chn mo_header number =
  let untranslated = 
    input_mo_untranslated ~failsafe chn mo_header number
  in
  let translated = 
    input_mo_translated ~failsafe chn mo_header number
  in
  match untranslated with
    [ id ] -> Singular ( id, String.concat "\000" translated )
  | id :: id_plural :: [] -> Plural ( id, id_plural, translated )
  | id :: id_plural :: tl ->
      fail_or_continue failsafe 
      (Junk (id, tl)) 
      (Plural (id, id_plural, translated))
  | [] ->
      fail_or_continue failsafe
      EmptyEntry
      (Singular ( "", ""))
;;

let get_translated_value ?(failsafe = Ignore) translation plural_number =
  match (translation, plural_number) with
    ((Singular (_,str)), 0) ->
      str
  | ((Singular (_,str)), x) ->
      fail_or_continue 
      failsafe
      (InvalidTranslationSingular(str,x))
      str
  | ((Plural (_,_,lst)), x) when x < List.length lst ->
      List.nth lst x 
  | ((Plural (_,_,lst)), x) ->
      fail_or_continue 
      failsafe
      (InvalidTranslationPlural(lst,x))
      List.nth lst 0
;;

let input_mo_informations ?(failsafe = Ignore) chn mo_header =
  (* La position de "" est forcément 0 *)
  let empty_translation = 
    get_translated_value ~failsafe 
    (input_mo_translation ~failsafe chn mo_header 0)
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

let output_mo ?(endianess = ArchEndian) chn lst =
  let null_terminated lst = 
    List.map ( fun str -> str^"\000" ) lst
  in
  let compute_table start_pos lst = 
    let compute_length lst = 
      List.map String.length lst
    in
    let compute_offset (current_pos,lst_pos) length = 
      ( current_pos + length, (current_pos,length) :: lst_pos )
    in
    let (final_pos, lst_rev) = 
      List.fold_left compute_offset (start_pos, []) (compute_length lst)
    in
    (final_pos, List.rev lst_rev)
  in
  let untranslated = 
    let to_string entry = 
      match entry with
        Singular (id, _) -> id
      | Plural (id, id_plural, _) -> id ^ "\000" ^ id_plural
    in
    null_terminated (List.map to_string lst)
  in
  let translated = 
    let to_string entry = 
      match entry with
        Singular (_,str) -> str
      | Plural (_, _, lst) -> String.concat "\000" lst
    in
    null_terminated (List.map to_string lst)
  in
  let gN = List.length lst
  in
  let gO = 28 (* Size of the header *)
  in
  let gT = gO + 8 * gN
  in
  let gS = 0 (* Hashtable is not implemented, since algorithm is not public -- documented *)
  in
  let gH = gT + 8 * gN
  in
  let (final_untranslated,untranslated_table) = 
    compute_table (gH + (gS+1) * 4) untranslated
  in
  let (_,translated_table) = 
    compute_table final_untranslated translated
  in
  let header = {
    endianess                = endianess;
    file_format_revision     = Int32.zero;
    number_of_strings        = Int32.of_int gN;
    offset_table_strings     = Int32.of_int gO;
    offset_table_translation = Int32.of_int gT;
    size_of_hashing_table    = Int32.of_int gS;
    offset_of_hashing_table  = Int32.of_int gH;
  }
  in
  output_mo_header chn header;
  List.iter (
    List.iter (
      fun (a,b) -> 
        output_int32_pair chn endianess (Int32.of_int a,Int32.of_int b) 
      ) 
  ) [ untranslated_table ; translated_table ];
  List.iter (output_string chn) untranslated;
  List.iter (output_string chn) translated
;;
