
open GettextTypes;;
open FileUtil.StrUtil;;

(** filename wich generates the error message str *)
exception ProblemReadingFile of filename * string;;
(** while extracting filename the command str returns exit code i *)
exception ExtractionFailed of filename * string * int;;
(** while extracting filename the command receive signal i *)
exception ExtractionInterrupted of filename * string * int;;

(** extract cmd default_option file_options src_files ppf : extract the
    translatable strings from all the src_files provided. Each source file will 
    be extracted using the command cmd, which should be an executable that has
    the same output as ocaml-xgettext. If cmd is not provided, it will be
    searched in the current path. The command will be called with
    default_option, or if the file being extracted is mapped in file_options,
    with the option associated to the filename in file_options. The result will
    be written using module Format to the formatter ppf. The result of the
    extraction should be used as a po template file.
  *)
type domain_key = 
    KeyDomain of string
  | KeyNoDomain
;;

module MapDomain = Map.Make(struct
  type t = domain_key
  let compare k1 k2 = 
    match (k1,k2) with
      KeyDomain s1, KeyDomain s2 -> compare s1 s2
    | KeyDomain s1, KeyNoDomain -> -1
    | KeyNoDomain, KeyDomain s2 -> 1
    | KeyNoDomain, KeyNoDomain -> 0
end);;

let add_list map lst =
  let key_value_of_entry entry =
    match entry with
      Domain (str,lst) -> (KeyDomain str,lst)
    | NoDomain(lst) -> (KeyNoDomain,lst)
  in
  let add_list_one map entry =
    let (key,value) = key_value_of_entry entry
    in
    let old_value = 
      try 
        MapDomain.find key map 
      with Not_found ->
        []
    in
    MapDomain.add key (List.rev_append value old_value) map
  in
  List.fold_left add_list_one map lst
;;

let to_list map =
  let entry_of_key_value key value =
    match key with 
      KeyDomain str -> Domain(str, value)
    | KeyNoDomain -> NoDomain(value)
  in
  MapDomain.fold ( 
    fun key value lst -> 
      (entry_of_key_value key value) :: lst
    ) map []
;;
  
let po_of_filename filename = 
  let chn = 
    try
      open_in filename
    with Sys_error(str) ->
      raise (ProblemReadingFile(filename,str))
  in
  let po = 
    GettextPo.input_po chn
  in
  close_in chn;
  po
;;

let extract command default_options filename_options filename_lst filename_pot =
  let make_command options filename = 
    command^" "^options^" "^filename
  in
  let extract_one map filename =
    let options = 
      try
        MapString.find filename filename_options 
      with Not_found ->
        default_options
    in
    let real_command = 
      make_command options filename
    in
    let chn = 
      Unix.open_process_in real_command
    in 
    let value = 
      (Marshal.from_channel chn : po_content list) 
    in
    match Unix.close_process_in chn with
    | Unix.WEXITED 0 ->
        add_list map value
    | Unix.WEXITED exit_code -> 
        raise (ExtractionFailed(filename,real_command,exit_code))
    | Unix.WSIGNALED signal
    | Unix.WSTOPPED signal -> 
        raise (ExtractionInterrupted(filename,real_command,signal))
  in
  let extraction = 
    List.fold_left extract_one MapDomain.empty filename_lst
  in
  let chn = 
    open_out filename_pot
  in
  GettextPo.output_po chn (to_list extraction);
  close_out chn
;;

(** compile *)
let compile filename_po filename_mo =
  let po = 
    po_of_filename filename_po
  in
  let output_domain key value =
    let real_filename_mo =
      match key with
        KeyDomain str ->
        str^"."^filename_mo
      | KeyNoDomain ->
        filename_mo
    in
    let chn = 
      open_out_bin real_filename_mo
    in
    GettextMo.output_mo chn value;
    close_out chn
  in
  let domain_merged = 
    add_list MapDomain.empty po
  in
  MapDomain.iter output_domain domain_merged
;;

let install destdir language category textdomain filename_mo_src =
  let filename_mo_dst = 
    GettextDomain.make_filename destdir language category textdomain
  in
  cp [filename_mo_src] filename_mo_dst
;;

(*let merge filename_pot filename_po_lst backup_extension =
  let pot = 
    po_of_filename filename_pot
  in
  let merge_one filename_po =
    let po = 
      po_of_filename filename_po
    in
    let _ = 
      (* BUG: should use add_extension *)
      mv filename_po (filename_po^"."^backup_extension)
    in
    
      
;;*)
