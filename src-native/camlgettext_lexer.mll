{

open Camlgettext_parser;;

let base = Int32.of_int 256
;;

let bendian_to_int c1 c2 c3 c4 =
	List.fold_left 
		(fun accu x -> Int32.add (Int32.mul accu base) (Int32.of_int (Char.code x))) 
		Int32.zero [c1;c2;c3;c4]
;;
	
let lendian_to_int c1 c2 c3 c4 =
	bendian_to_int c4 c3 c2 c1
;;
}

rule
token_header = parse
  '\x95' '\x04' '\x12' '\xde'  { BENDIAN }
| '\xde' '\x12' '\x04' '\x95'  { LENDIAN } 
and
token_number str_to_int = parse
 (. as c1) (. as c2) (. as c3) (. as c4)  { (INT (str_to_int c1 c2 c3 c4)) }
   
and
token_string = parse
  [^'\000']* as str            { (STRING str) }
| '\000'                       { NUL }  
