type token =
  | COLON
  | LINE of (int)
  | KEYWORD of (string)
  | FILENAME of (string)
  | COMMENT_EOF

val comment_filepos :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> GettextTypes.po_filepos list
val comment_special :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> GettextTypes.po_special list
