{

  open GettextFormat_parser;;

}

let flags = ['-''0''+'' ']* ['0'-'9']* ['.' ['0'-'9']*]?

rule
token = parse
 '%' flags    { format_char lexbuf }
| _           { token lexbuf }
and
format_char = 
  "d" as sf
| "i" as sf
| "n" as sf
| "N" as sf              { ("d", sf) }
| "u" as sf 
| "x" as sf 
| "X" as sf              { ("u", sf) }
| "o"  as sf                  { } 
| "s"  as sf                  { }
| "S"  as sf                  { }
| "c"  as sf                  { }
| "C"  as sf                  { }
| "f"  as sf
| "F"  as sf
| "e"    as sf                { } 
| "E"    as sf
| "g"    as sf
| "G"    as sf                { }
| "B"    as sf
| "b"    as sf                { }
| "ld"   as sf
| "li"  as sf                 { }
| "lu"  as sf                 { }
| "lx"  as sf
| "lX"  as sf                 { }
| "lo"  as sf                 { }
| "nd"  as sf
| "ni"  as sf                 { }
| "nu"   as sf                { }
| "nx"   as sf
| "nX"   as sf                { }
| "no"   as sf                { }
| "Ld"   as sf
| "Li"   as sf                { }
| "Lu"   as sf                { }
| "Lx" as sf
| "LX" as sf                  { }
| "Lo" as sf                  { }
| "a"  as sf                  { }
| "t"  as sf                  { }
| '!'  as sf                  { }
| '%'  as sf                  { }
;
