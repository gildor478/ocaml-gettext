%{

open GettextTypes;;

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

%type < GettextTypes.po_content_type list > msgfmt
%start msgfmt

%%

msgfmt:
  msgfmt domain        { let (a,b) = $2 in Domain (a,b) :: $1 } 
| domain               { let (a,b) = $1 in [Domain (a,b)] }
| msgfmt message       { $2 :: $1 }
| message              { [$1] }
| EOF                  { [] }
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
  MSGID string_list MSGSTR string_list               { SingularEntry($2,$4) } 
| MSGID string_list msgid_pluralform pluralform_list { PluralEntry($2,$3,$4) }
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
