
open GettextTypes;;
open FileUtil;;
open FileUtil.StrUtil;;
open FilePath.DefaultPath;;

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
let extract command default_options filename_options filename_lst filename_pot =
  let make_command options filename = 
    Printf.sprintf "%s %s %s" command options filename
  in
  let extract_one po filename =
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
      (Marshal.from_channel chn : po_content) 
    in
    match Unix.close_process_in chn with
    | Unix.WEXITED 0 ->
        GettextPo.merge_po po value
    | Unix.WEXITED exit_code -> 
        raise (ExtractionFailed(filename,real_command,exit_code))
    | Unix.WSIGNALED signal
    | Unix.WSTOPPED signal -> 
        raise (ExtractionInterrupted(filename,real_command,signal))
  in
  let extraction = 
    List.fold_left extract_one GettextPo.empty_po filename_lst
  in
  let chn = 
    open_out filename_pot
  in
  let date =
    let current_time = 
      Unix.time ()
    in
    let gmt_time = 
      Unix.gmtime current_time
    in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d+0000"
    (gmt_time.Unix.tm_year + 1900) 
    (gmt_time.Unix.tm_mon + 1)
    (gmt_time.Unix.tm_mday)
    (gmt_time.Unix.tm_hour)
    (gmt_time.Unix.tm_min)
  in
  Printf.fprintf chn 
"# SOME DESCRIPTIVE TITLE.
# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER
# This file is distributed under the same license as the PACKAGE package.
# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.
#
#, fuzzy
msgid \"\"
msgstr \"\"
\"Project-Id-Version: PACKAGE VERSION\\n\"
\"Report-Msgid-Bugs-To: \\n\"
\"POT-Creation-Date: %s\\n\"
\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"
\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"
\"Language-Team: LANGUAGE <LL@li.org>\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=CHARSET\\n\"
\"Content-Transfer-Encoding: 8bit\\n\"
\"Plural-Forms: nplurals=INTEGER; plural=EXPRESSION;\\n\"

" date;
  GettextPo.output_po chn extraction;
  close_out chn
;;

(** compile input_po output_mo : create a binary representation of the PO file
    provided as input_pot. The output file is output_mo. 
*)
let compile filename_po filename_mo =
  let po = 
    po_of_filename filename_po
  in
  let output_one_map filename map = 
    let lst = 
      MapString.fold ( fun _ (_,e) lst -> 
        (GettextPo.translation_of_po_translation e) :: lst 
      ) map []
    in
    let chn = 
      open_out_bin filename
    in
    GettextMo.output_mo chn lst;
    close_out chn
  in
  let make_filename domain filename_mo =
    let dirname = 
      dirname filename_mo
    in
    let basename =
      basename filename_mo
    in
    (* BUG : should use add_extension *)
    make_filename [ dirname ; domain^"."^basename ]
  in
  output_one_map filename_mo po.no_domain;
  MapTextdomain.iter ( 
    fun domain map -> 
      output_one_map (make_filename domain filename_mo) map 
    ) po.domain
;;

(** install destdir language category textdomain fln : copy the given
    filename ( should be a MO file ) to the filename defined by all the
    other parameters ( typically destdir/language/category/textdomain.mo ).
*)
let install destdir language category textdomain filename_mo_src =
  let filename_mo_dst = 
    GettextDomain.make_filename destdir language category textdomain
  in
  let dirname_mo_dst =
    dirname filename_mo_dst
  in
  (* Test of the mo file, it will raise an exception if there is any problem 
     in the MO structure *)
  let chn = 
    open_in_bin filename_mo_src 
  in
  let mo_hdr =
    GettextMo.input_mo_header chn
  in
  let mo_info =
    GettextMo.input_mo_informations RaiseException chn mo_hdr
  in
  for i = 0 to (Int32.to_int mo_hdr.number_of_strings) - 1 do
    let _ = GettextMo.input_mo_translation RaiseException chn mo_hdr i
    in
    ()
  done;
  mkdir ~parent:true dirname_mo_dst;
  cp [filename_mo_src] filename_mo_dst
;;

(** uninstall orgdir language category textdomain : remove the MO file 
    defined by all the other parameters 
    ( typically destdir/language/category/textdomain.mo ).
*)
let uninstall orgdir language category textdomain =
  let filename_mo_org = 
    GettextDomain.make_filename orgdir language category textdomain
  in
  if test Exists filename_mo_org then
    rm [filename_mo_org]
  else
    ()
;;

(** merge fln_pot fln_po_lst backup_ext : use fln_pot as a POT file and
    merge the current content of the listed PO file ( fln_po_lst ) with it.
    Backup all the PO file using the provided backup extension backup_ext and 
    produce a merged PO file in place.
*)
let merge filename_pot filename_po_lst backup_extension =
  let pot = 
    po_of_filename filename_pot
  in
  let merge_one filename_po =
    let po = 
      po_of_filename filename_po
    in
    let po_merged = 
      GettextPo.merge_pot pot po
    in
    let _ = 
      (* BUG: should use add_extension *)
      (* BUG: should use mv *)
      Sys.rename filename_po (filename_po^"."^backup_extension)
    in
    let chn = 
      open_out filename_po
    in
    GettextPo.output_po chn po_merged;
    close_out chn
  in
  List.iter merge_one filename_po_lst
;;
