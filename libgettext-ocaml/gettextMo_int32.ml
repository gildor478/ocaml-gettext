open GettextTypes;;

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

let byte_from_int32 i = 
  let one_byte = Int32.of_int 0xF
  in
  let extract_byte sb = 
    let mask = 
      Int32.shift_left one_byte ( sb * 8 ) 
    in
    let i_masked = 
      Int32.logand i mask
    in
    Int32.to_int (Int32.shift_right i_masked ( sb * 8 ))
  in
  ( extract_byte 3, extract_byte 2, extract_byte 1, extract_byte 0 )
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

let output_int32 chn endian vl =
  let (a0,a1,a2,a3) = 
    byte_from_int32 vl
  in
  let order = 
    match endian with
      ArchEndian ->
        [a0;a1;a2;a3]
    | NotArchEndian ->
        [a3;a2;a1;a0]
  in
  List.iter (output_byte chn) order
;;

let input_int32_pair chn endian =  
   let a = input_int32 chn endian
   in
   let b = input_int32 chn endian
   in
   (a, b)
;;

let output_int32_pair chn endian (a,b) = 
  output_int32 chn endian a;
  output_int32 chn endian b
;;

let input_int32_pair_string chn endian =
   let (length,offset) = 
     input_int32_pair chn endian
   in
   let str = String.make (Int32.to_int length) 'X'
   in
   seek_in chn (Int32.to_int offset);
   really_input chn str 0 (Int32.to_int length);
   str
;;
