%{

open GettextTypes;;
open GettextLocale_types;;
 
%}

%token UNDERSCORE
%token DOT
%token AT
%token EOF
%token <string> ID

%start main
%type <GettextLocale_types.locale> main
%%

main:
  locale EOF          { (*print_endline "eof";*) $1 }
;

locale:
| locale UNDERSCORE ID  { (*print_endline "underscore";*) { $1 with territory = Some $3 } }
| locale DOT ID         { (*print_endline "dot";*) { $1 with codeset = Some $3 } }
| locale AT ID          { (*print_endline "at";*) { $1 with modifier = Some $3 } }
| ID                    { (*print_endline "id";*) create_locale $1 }
;
