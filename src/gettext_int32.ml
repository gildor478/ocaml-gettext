open Camlgettext_types;;

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
 
