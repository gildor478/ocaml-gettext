open GettextStub

let _ =
  (* Sets the environnement variable using the
     first argument of the program.
     This is done for testing i18n. *)
  print_endline "Putenv";
  Unix.putenv "LC_ALL" (Array.get Sys.argv 1)
in

let _ =
  (* Sets the locale using th environnement variable *)
  print_endline "Setlocale";
  print_endline (setlocale LC_ALL "")
in

let _ =
  (* Define the prefix name of the .mo files (usually the prgram name)  *)
  print_endline "Textdomain";
  textdomain "test"
in
let _ =
  (* Give the locale files' path. *)
  (* For instance the .mo file corresponding to french
  should be ./locale/fr/LC_MESSAGES/test.mo *)
  print_endline "Bindtextdomain";
  bindtextdomain "test" "./locale/"
in

print_endline (_ "'Your command, please?', asked the waiter.")
