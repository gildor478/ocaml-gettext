{

open GettextPo_parser;;

}

rule
token = parse
  "msgstr" { MSGSTR }
| "msgid"  { MSGID }
| "msgid_plural" { MSGID_PLURAL }
| "domain"       { DOMAIN }
| '['            { LBRACKET }
| ']'            { RBRACKET }
| ['0'-'9']+ as nbr { NUMBER ( int_of_string nbr) }
| '"'               { STRING (string_val lexbuf) }
| eof               { EOF }
| '#'               { comment_skip lexbuf }
| [' ''\t''\r''\n'] { token lexbuf }
and
string_val = parse
  '"'             { "" }
| [^'"']+ as str  { str ^ (string_val lexbuf) }
and
comment_skip = parse
 '\n'          { token lexbuf }
| _            { comment_skip lexbuf }

