
let read_po_file chn =
  let lexbuf = Lexing.from_channel chn
  in
  GettextPo_parser.msgfmt GettextPo_lexer.token lexbuf
