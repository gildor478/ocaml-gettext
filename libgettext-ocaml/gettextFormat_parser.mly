%{

%}

%token <string> FORMAT_CHAR 
%token EOF

%type < string list > main
%start main

%%

main:
  format_char EOF { List.reverse $1 }
;

format_char: 
  format_char FORMAT_CHAR { $2 :: $1 }
|                         { [] }
;

%%
