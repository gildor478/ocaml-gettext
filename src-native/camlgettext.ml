
type endianess = ArchEndian | NotArchEndian ;;

exception Bad_mo_file ;;

let int32_from_byte (a0,a1,a2,a3) = 
	Int32.add 
	  (Int32.shift_left (Int32.of_int a0) 24) 
	  (Int32.of_int 
	    ( 
	      (a1 lsl 16) + 
	      (a2 lsl  8) +
	      a3
	    )
	  )
;;

let input_int32 chn endian = 
  let (a0,a1,a2,a3) = 
    (
      input_byte chn, 
      input_byte chn, 
      input_byte chn, 
      input_byte chn 
    )
  in
  match endian with 
    ArchEndian ->
      int32_from_byte (a0,a1,a2,a3)
  | NotArchEndian ->
      int32_from_byte (a3,a2,a1,a0)
;;


let input_int32_pair chn endian =  
   let a = input_int32 endian chn
   in
   let b = input_int32 endian chn
   in
   (a, b)
;;

let input_int32_pair_table chn endian number =
   let rec input_int32_pair_table_aux number lst = 
     match number with 
       0 -> lst
     | x -> input_int32_pair_table_aux 
       (x-1) 
       ((input_int32_pair endian chn) :: lst)
   in
   input_int32_pair_table_aux number []
;;
	
(*
   From GNU Gettext documentation 
   ( http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC136 ).

   Format of MO file :

        byte
             +------------------------------------------+
          0  | magic number = 0x950412de                |
             |                                          |
          4  | file format revision = 0                 |
             |                                          |
          8  | number of strings                        |  == N
             |                                          |
         12  | offset of table with original strings    |  == O
             |                                          |
         16  | offset of table with translation strings |  == T
             |                                          |
         20  | size of hashing table                    |  == S
             |                                          |
         24  | offset of hashing table                  |  == H
             |                                          |
             .                                          .
             .    (possibly more entries later)         .
             .                                          .
             |                                          |
          O  | length & offset 0th string  ----------------.
      O + 8  | length & offset 1st string  ------------------.
              ...                                    ...   | |
O + ((N-1)*8)| length & offset (N-1)th string           |  | |
             |                                          |  | |
          T  | length & offset 0th translation  ---------------.
      T + 8  | length & offset 1st translation  -----------------.
              ...                                    ...   | | | |
T + ((N-1)*8)| length & offset (N-1)th translation      |  | | | |
             |                                          |  | | | |
          H  | start hash table                         |  | | | |
              ...                                    ...   | | | |
  H + S * 4  | end hash table                           |  | | | |
             |                                          |  | | | |
             | NUL terminated 0th string  <----------------' | | |
             |                                          |    | | |
             | NUL terminated 1st string  <------------------' | |
             |                                          |      | |
              ...                                    ...       | |
             |                                          |      | |
             | NUL terminated 0th translation  <---------------' |
             |                                          |        |
             | NUL terminated 1st translation  <-----------------'
             |                                          |
              ...                                    ...
             |                                          |
             +------------------------------------------+

*)

type mo_header_type = {
  endianess                : endianess;
  file_format_revision     : int32;
  number_of_strings        : int32;
  offset_table_strings     : int32;
  offset_table_translation : int32;
  size_of_hashing_table    : int32;
  offset_of_hashing_table  : int32;
}
;;

type mo_translation_type = {
  translation_from_string : (string, string) Hashtbl.t;
}
;;

let input_mo_header chn = 
  let endianess = 
    let magic_number = seek_in chn 0; input_int32 chn ArchEndian
    in
    match Int32.to_int magic_number with
      0x950412de ->
        ArchEndian
    | 0xde120495 ->
        NotArchEndian
    | _ ->
          raise Bad_mo_file 
  in
  let seek_and_input x = seek_in chn x; input_int32 chn endianess
  in
  {
    endianess                = endianess;
    file_format_revision     = seek_and_input  4;
    number_of_strings        = seek_and_input  8;
    offset_table_strings     = seek_and_input 12;
    offset_table_translation = seek_and_input 16;
    size_of_hashing_table    = seek_and_input 20;
    offset_of_hashing_table  = seek_and_input 24;
  }
;;

let string_of_mo_header mo_header = 
  let buff = Buffer.create 256
  in
  Printf.bprintf buff "File format revision                     : %ld\n" mo_header.file_format_revision;
  Printf.bprintf buff "Number of string                         : %ld\n" mo_header.number_of_strings;
  Printf.bprintf buff "Offset of table with original strings    : %lx\n" mo_header.offset_table_strings;
  Printf.bprintf buff "Offset of table with translation strings : %lx\n" mo_header.offset_table_translation;
  Printf.bprintf buff "Size of hashing table                    : %lx\n" mo_header.size_of_hashing_table;
  Printf.bprintf buff "Offset of hashing table                  : %lx\n" mo_header.offset_of_hashing_table;
  Buffer.contents buff
;;

let input_mo_translation chn mo_header =
  let input_strings_list () = 
    let input_one_string (length,offset) = 
      let str = String.make (Int32.to_int length) 'X'
      in
      seek_in chn (Int32.to_int offset);
      really_input chn str 0 (Int32.to_int length);
      str
    in
    List.map input_one_string
      (
        input_int32_pair_table 
	  chn 
          mo_header.endianess 
	  (Int32.to_int mo_header.number_of_strings) 
      )
   in
   let list_strings = 
     seek_in chn (Int32.to_int mo_header.offset_table_strings);
     input_strings_list ()
   in
   let list_translations = 
     seek_in chn (Int32.to_int mo_header.offset_table_translation);
     input_strings_list ()
   in
   let translations = Hashtbl.create (Int32.to_int mo_header.number_of_strings)
   in
   let _ = List.iter2 
     ( fun s t -> Hashtbl.add translations s t ) 
     list_strings list_translations
   in
   {
     translation_from_string = translations;
   }
;;

let string_of_mo_translation mo_translation = 
  let buff = Buffer.create 1024
  in
  Printf.bprintf buff "Translations : \n";
  Hashtbl.iter ( fun s t -> Printf.bprintf buff "%s -> %s\n" s t ) mo_translation.translation_from_string;
  Buffer.contents buff
;;

let mo_file = open_in_bin "messages.mo"
in
let header = input_mo_header mo_file
in
let translation = input_mo_translation mo_file header
in
print_string (string_of_mo_header header);
print_newline ();
print_string (string_of_mo_translation translation);
print_newline ()
;;
