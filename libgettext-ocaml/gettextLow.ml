(* Gettext nearby function ( use this one to have the closest API to gettext *)

let format_of_string = 
  Obj.magic
;;

let gettext t' str =
  t' None str None Locale.messages
;;

let fgettext t' str =
  format_of_string gettext
;;
  
let dgettext t' textdomain str =
  t' (Some domain) str None Locale.messages
;;

let fdgettext t' domain str =
  format_of_string dgettext
;;
  
let dcgettext t' textdomain str category = 
  t' (Some domain) str None category
;;

let fdcgettext t' domain str category =
  format_of_string dcgettext
;;
  
let ngettext t' str str_plural n =
  t' None str (Some(str_plural,n)) Locale.messages
;;

let fngettext t' str str_plural n =
  format_of_string ngettext
;;
  
let dngettext t' domain str str_plural n =
  t' (Some domain) str (Some (str_plural,n)) Locale.messages
;;

let fdngettext t' domain str str_plural n =
  format_of_string dngettext
;;

let dcngettext t' domain str str_plural n category = 
  t' (Some domain) str (Some(str_plural,n)) category
;;
  
let fdcngettext =
  format_of_string dcngettext
;;
