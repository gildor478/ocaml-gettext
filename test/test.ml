open Camlgettext;;

let _ = setlocale LC_ALL ""
in
print_string (gettext "Coucou");
print_newline ();;

