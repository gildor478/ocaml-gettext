{
open GettextLocale_parser;;
}

rule token = parse
    '_'                 { UNDERSCORE }
  | '.'                 { DOT }
  | '@'                 { AT }
  | eof                 { EOF }
  | [^'_''.''@']* as id { ID(id) }

