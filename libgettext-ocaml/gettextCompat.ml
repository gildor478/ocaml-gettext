
open GettextTypes;;
open GettextModules;;

let format_of_string = 
  Obj.magic
;;

let bindtextdomain textdomain dir t =
  upgrade_textdomain t textdomain (None,Some dir)
;;
  
let bind_textdomain_codeset textdomain codeset t =
  upgrade_textdomain t textdomain (Some codeset,None)
;;

let textdomain default t = 
  { t with default = default }
;;

let get_textdomain t = 
  t.default
;;

let gettext t' str =
  t' None str None LC_MESSAGES
;;

let fgettext t' str =
  format_of_string gettext
;;
  
let dgettext t' textdomain str =
  t' (Some textdomain) str None LC_MESSAGES
;;

let fdgettext t' domain str =
  format_of_string dgettext
;;
  
let dcgettext t' textdomain str category = 
  t' (Some textdomain) str None category
;;

let fdcgettext t' domain str category =
  format_of_string dcgettext
;;
  
let ngettext t' str str_plural n =
  t' None str (Some(str_plural,n)) LC_MESSAGES
;;

let fngettext t' str str_plural n =
  format_of_string ngettext
;;
  
let dngettext t' textdomain str str_plural n =
  t' (Some textdomain) str (Some (str_plural,n)) LC_MESSAGES
;;

let fdngettext t' domain str str_plural n =
  format_of_string dngettext
;;

let dcngettext t' textdomain str str_plural n category = 
  t' (Some textdomain) str (Some(str_plural,n)) category
;;
  
let fdcngettext =
  format_of_string dcngettext
;;
