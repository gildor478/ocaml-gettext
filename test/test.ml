open Unix;;
open Camlgettext;;

let _ = 
	print_string "Putenv";
	print_newline ();
	putenv "LC_ALL" (Array.get Sys.argv 1)
in
let _ = 
	print_string "Setlocale";
	print_newline ();
	setlocale LC_ALL ""
in
let _ = 
	print_string "Textdomain";
	print_newline ();
	textdomain "prog"
in
let _ = 
	print_string "Bindtextdomain";
	print_newline ();
	bindtextdomain "prog" "."
in
print_string (_"'Your command, please?', asked the waiter.");
print_newline ()
