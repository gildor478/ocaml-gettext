
open ProgramGettext;;

let () = 
  let my_name = ref ""
  in
  let spf x = Printf.sprintf x
  in
  let (gettext_args,gettext_copyright) =
    Gettext.init
  in
  let args =
    Arg.align (
      [
        "--my-name",
        Arg.String ( fun s ->
          my_name := s
        ),
        ( spf (f_ "name Your name. Default : %S") !my_name )
      ] @ gettext_args
    )
  in
  let () =
    Arg.parse
    args
    ( fun str -> () )
    (
      spf (f_ 
"\"Hello you\" program by Sylvain Le Gall

%s

Command: program [options]

Options:") gettext_copyright
    )
  in
  Library.hello_you !my_name;
  Gui.hello_you !my_name
;;
