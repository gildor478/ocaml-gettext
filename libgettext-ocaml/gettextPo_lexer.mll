{

open GettextPo_parser;;

let next_line lexbuf = 
  lexbuf.Lexing.lex_curr_p <-
  {
    lexbuf.Lexing.lex_curr_p with
    Lexing.pos_lnum = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 1;
    Lexing.pos_bol  = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
  }
;;

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
| '#'                        { comment_skip lexbuf }
| [' ''\t']                  { token lexbuf }
| ['\r''\n']                 { next_line lexbuf; token lexbuf }
| ([^' ''\t''\r''\n''"'':''0'-'9''['']''#'][^' ''\t''\r''\n''"'':''['']''#']*) as str 
                             { FILENAME(str) }
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
and
comment_skip = parse
 '\n'          { next_line lexbuf; token lexbuf }
| _            { comment_skip lexbuf }

