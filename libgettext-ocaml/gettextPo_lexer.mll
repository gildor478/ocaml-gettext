{

open GettextPo_parser;;

}

rule
token = parse
  "msgstr"                   { MSGSTR }
| "msgid"                    { MSGID }
| "msgid_plural"             { MSGID_PLURAL }
| "domain"                   { DOMAIN }
| '['                        { LBRACKET }
| ']'                        { RBRACKET }
| ':'                        { COLON }
| ['0'-'9']+ as nbr          { NUMBER (int_of_string nbr) }
| '"'                        { STRING (string_val lexbuf) }
| eof                        { EOF }
| "#:"                       { COMMENT_LOCATION }
| '#' ([^'\n']* as str) '\n' { COMMENT(str) }
| [^' ''\t''\r''\n']* as str { FILENAME(str) }
| [' ''\t''\r''\n']          { token lexbuf }
and
string_val = parse
  "\\n"              { "\n" ^ ( string_val lexbuf) } 
| "\\t"              { "\t" ^ ( string_val lexbuf) }
| "\\b"              { "\b" ^ ( string_val lexbuf) }
| "\\r"              { "\r" ^ ( string_val lexbuf) }
| "\\f"              { "\012" ^ ( string_val lexbuf) }
| "\\v"              { "\011" ^ ( string_val lexbuf) }
| "\\a"              { "\007" ^ ( string_val lexbuf) } 
| "\\\""             { "\"" ^ ( string_val lexbuf) }
| "\\\\"             { "\\" ^ ( string_val lexbuf) }
| '\\' (['0'-'7'] ['0'-'7']? ['0'-'7']?) as oct
                     { 
                       let chr = 
                         try 
                           char_of_int (int_of_string ( "0o" ^ oct ))
                         with _ ->
                           char_of_int 255
                       in
                       ( String.make 1 chr ) ^ ( string_val lexbuf )
                     }
| "\\x" (['0'-'9''A'-'F''a'-'f'] ['0'-'9''A'-'F''a'-'f']?) as hex
                     {
                       let chr = 
                         try
                           char_of_int (int_of_string ("0x" ^ hex ))
                         with _ ->
                           char_of_int 255
                       in
                       ( String.make 1 chr ) ^ ( string_val lexbuf )
                     }
| [^'"''\\']+ as str { str ^ (string_val lexbuf) }
| '"'                { "" }
