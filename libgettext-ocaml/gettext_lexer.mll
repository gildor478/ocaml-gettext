{

open Camlgettext_parser;;

}

rule
token_field_name = parse
  "Content-Type" [' ''\t']* ':'      { CONTENT_TYPE(token_field_value lexbuf) }
| "Plural-Forms" [' ''\t']* ':'      { PLURAL_FORMS(token_field_value lexbuf) } 
| ([^'\n''\r''\t'' ']+ as id) [' ''\t']* ':' { FIELD_NAME(id, token_field_value lexbuf) }
| eof                                { EOF }
| _                                  { token_field_name lexbuf}
and
token_field_value = parse
  [^'\n''\r']* as str           { str }
and
token_field_plural_value = parse
  "nplurals"                    { NPLURALS }
| ';'                           { SEMICOLON }
| "plural"                      { PLURAL }
|  "?"                          { QUESTION_MARK }
| ":"                           { COLON }
| "||"                          { OR }
| "&&"                          { AND }
| "=="                          { EQ }
| '='                           { EQUAL }
| "!="                          { NEQ }
| "<="                          { LE }
| "<"                           { L }
| ">="                          { GE }
| ">"                           { G }
| "+"                           { PLUS }
| "-"                           { MINUS }
| "*"                           { MUL }
| "/"                           { DIV }
| "%"                           { MOD }
| "!"                           { NOT }
| '('                           { LPAREN }
| ')'                           { RPAREN }
| "n"                           { ID }
| ['0'-'9']+ as nbr             { (NUMBER (int_of_string nbr) ) }
| eof                           { EOF }
| [' ''\t']                     { token_field_plural_value lexbuf }
and
token_field_content_type = parse
  "charset"                     { CHARSET }
| ';'                           { SEMICOLON }
| '='                           { EQUAL }
| [^' ''\t'';''=']+ as str      { (STRING str) }
| [' ''\t']                     { token_field_content_type lexbuf }
| eof                           { EOF }
