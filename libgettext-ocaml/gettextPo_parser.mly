%{

open GettextTypes;;
open GettextUtils;;
open GettextPo_utils;;

let check_string_format ref str =
  str
;;

let check_plural id id_plural lst =
  let rec check_plural_one nxt lst = 
    match lst with 
      (x,str) :: tl when x = nxt ->
        (check_string_format id str) :: (check_plural_one (nxt + 1) tl)
    | (x,str) :: tl ->
        raise (InvalidIndex(id,x))
    | [] ->
        []
  in
  (* No location, since i won't parse comments to find the previous one *)
  ([],Plural(id, (check_string_format id id_plural), (check_plural_one 0 lst)))
;;
  
let check_singular id str =
  (* No location, since i won't parse comments to find the previous one *)
  ([],Singular(id, check_string_format id str))
;;

%}

%token MSGSTR
%token MSGID
%token MSGID_PLURAL
%token DOMAIN
%token LBRACKET
%token RBRACKET
%token <int> NUMBER
%token <string> STRING
%token EOF

%type < GettextTypes.po_content > msgfmt
%start msgfmt

%%

msgfmt:
  msgfmt domain        { let (d,l) = $2 in List.fold_left (add_po_translation_domain d) $1 l } 
| domain               { let (d,l) = $1 in List.fold_left (add_po_translation_domain d) empty_po l }
| msgfmt message_list  { List.fold_left add_po_translation_no_domain $1 $2 }
| message_list         { List.fold_left add_po_translation_no_domain empty_po $1 }
| EOF                  { empty_po }
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
  MSGID string_list MSGSTR string_list               { check_singular $2 $4 } 
| MSGID string_list msgid_pluralform pluralform_list { check_plural $2 $3 (List.rev $4) }
;

msgid_pluralform:
  MSGID_PLURAL string_list { $2 }
;

pluralform_list:
  pluralform_list pluralform  { $2 :: $1 }
| pluralform                  { [$1] }
;

pluralform:
  MSGSTR LBRACKET NUMBER RBRACKET string_list { ($3,$5) }
;

string_list:
  string_list STRING { $1 ^ $2 }
| STRING             { $1 }
;
