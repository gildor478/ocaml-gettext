(** Helper program to : 
  - extract translatable strings from OCaml source,
  - compile PO file,
  - install MO file,
  - merge POT and PO file.
*)

module Gettext = Gettext.Program(struct
  let textdomain   = "ocaml-gettext"
  let codeset      = None
  let dir          = None
  let dependencies = Gettext.init
  let realize      = GettextCamomile.Open.realize
end)
;;

open Gettext;;

type action = 
    Extract
  | Compile
  | Install 
  | Merge
;;

type t = 
  {
    action_opt : action option;

  }
;;

let args = 
  let (gettext_args,gettext_copyright) = 
    init
  in
  let spf x = 
    Printf.sprintf x
  in
  let t = ref
    {
      action_option              = None;
      extract_command            = "ocaml-xgettext";
      extract_default_option     = "-I +camlp4 pa_o.cmo";
      extract_filename_options   = [];
      extract_pot_option         = None;
      extract_output_file_option = None;
      install_language_option    = None;
      install_category           = LC_MESSAGES;
      install_textdomain_option  = None;
      install_destdir            = GettextConfig.default_dir;
      merge_filename_pot_option  = None;
      merge_backup_extension     = "bak";
      input_files                = [];
    }
  in
  let actions = [
      "extract", Extract;
      "compile", Compile;
      "install", Install;
      "merge",   Merge
    ]
  in
  [
    (
      "-action",
      Arg.Symbol 
      (
        (List.map fst actions),
        (fun symbol ->
          try
            t := { !t with action_option = Some (List.assoc symbol) }
          with Not_found ->
            raise (Arg.BadArg (spf (f_ "Invalid action: %s") symbol))
        )
      ),
      (
        spf (f_ "Action to execute, could be %s. Default: none") 
        (String.concat " or " (List.map fst actions))
      )
    );
    (
      "-extract-command",
      Arg.String ( fun cmd ->
        t := { !t with extract_command = cmd }
      ),
      (
        spf (f_ "Command to extract translatable strings from an OCaml source file. Default: %s")
        !t.extract_command
      )
    );
    (
      "-extract-default-option",
      Arg.String ( fun default_option ->
        t := { !t with extract_default_option = default_option }
      ),
      (
        spf (f_ "Default option used when extracting translatable strings.  Default: %S")
        !t.extract_default_option
      )
    );
    (
      "-extract-filename-option",
      Arg.Tuple (
        let filename = ref ""
        in
        [
          Arg.String ( fun str -> filename := str );
          Arg.String ( fun options -> 
            t := { !t with extract_filename_options =
              (!filename,options) ::
              !t.extract_filename_options
            }
          )
        ]
      ),
      (
        spf (f_ "Per filename option used when extracting strings from the specified filename. Default: %s")
        (string_of_list !t.extract_filename_options)
      )
    );
    (
      "-extract-pot",
      (
        Arg.String ( fun str ->
          t := { !t with extract_filename_options = Some str }
        )
      ),
      (s_ "POT file to write when extracting translatable strings. Default: output to screen")
    );
    (
      "-"
      extract_output_file_option = None;
      install_language_option    = None;
      install_category           = LC_MESSAGES;
      install_textdomain_option  = None;
      install_destdir            = GettextConfig.default_dir;
      merge_filename_pot_option  = None;
      merge_backup_extension     = "bak";
      input_files                = [];
       
           
  ]
  "Ocaml-gettext
