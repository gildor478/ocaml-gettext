%{

open GettextTypes;;
open GettextUtils;;
open GettextPo_utils;;

let check_string_format ref str =
  str
;;

let check_plural locations id id_plural lst =
  let check_plural_one index lst = 
    List.rev (
      snd ( 
        List.fold_left ( fun (index,lst) (cur_index,cur_elem) ->
          if index + 1 = cur_index then
            (cur_index, (check_string_format id cur_elem) :: lst)
          else
            raise (InvalidIndex(String.concat "" id,cur_index))
        ) (index,[]) lst
      )
    )
  in
  (locations, PoPlural(id, (check_string_format id id_plural), (check_plural_one (-1) lst)))
;;
  
let check_singular locations id str =
  (* No location, since i won't parse comments to find the previous one *)
  (locations, PoSingular(id, check_string_format id str))
;;

%}

%token MSGSTR
%token MSGID
%token MSGID_PLURAL
%token DOMAIN
%token LBRACKET
%token RBRACKET
%token COLON
%token <int> NUMBER
%token <string> STRING
%token EOF
%token COMMENT_LOCATION
%token <string> FILENAME

%type < GettextTypes.po_content > msgfmt
%start msgfmt

%%

msgfmt:
  msgfmt domain         { let (d,l) = $2 in List.fold_left (add_po_translation_domain d) $1 l } 
| domain                { let (d,l) = $1 in List.fold_left (add_po_translation_domain d) empty_po l }
| msgfmt message_list   { List.fold_left add_po_translation_no_domain $1 $2 }
| message_list          { List.fold_left add_po_translation_no_domain empty_po $1 }
| EOF                   { empty_po }
;

comment_location:
  COMMENT_LOCATION location_list { $2 }
;

location_list:
  location_list location   { $2 :: $1 }
| location                 { [$1] }
;

location:
  FILENAME COLON NUMBER    { ($1,$3) }
;

domain:
  DOMAIN STRING message_list { ($2,$3) }
| DOMAIN STRING              { ($2,[]) }
;

message_list:
  message_list message { $2 :: $1 }
| message              { [$1] }
;

message:
  comment_location MSGID string_list MSGSTR string_list               
    { check_singular $1 (List.rev $3) (List.rev $5) } 
| MSGID string_list MSGSTR string_list               
    { check_singular [] (List.rev $2) (List.rev $4) } 
| comment_location MSGID string_list msgid_pluralform pluralform_list 
    { check_plural $1 (List.rev $3) $4 (List.rev $5) }
| MSGID string_list msgid_pluralform pluralform_list 
    { check_plural [] (List.rev $2) $3 (List.rev $4) }
;

msgid_pluralform:
  MSGID_PLURAL string_list { (List.rev $2) }
;

pluralform_list:
  pluralform_list pluralform  { $2 :: $1 }
| pluralform                  { [$1] }
;

pluralform:
  MSGSTR LBRACKET NUMBER RBRACKET string_list { ($3,(List.rev $5)) }
;

string_list:
  string_list STRING { $2 :: $1 }
| STRING             { [$1] }
;
