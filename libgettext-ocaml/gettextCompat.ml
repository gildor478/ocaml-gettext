
open GettextTypes;;
open GettextModules;;

let format_of_string x = 
  Obj.magic x
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
  t' false None str None LC_MESSAGES
;;

let fgettext t' str =
  format_of_string (t' true None str None LC_MESSAGES)
;;
  
let dgettext t' textdomain str =
  t' false (Some textdomain) str None LC_MESSAGES
;;

let fdgettext t' textdomain str =
  format_of_string (t' false (Some textdomain) str None LC_MESSAGES)
;;
  
let dcgettext t' textdomain str category = 
  t' false (Some textdomain) str None category
;;

let fdcgettext t' textdomain str category =
  format_of_string (t' true (Some textdomain) str None category)
;;
  
let ngettext t' str str_plural n =
  t' false None str (Some(str_plural,n)) LC_MESSAGES
;;

let fngettext t' str str_plural n =
  format_of_string (t' true None str (Some(str_plural,n)) LC_MESSAGES)
;;
  
let dngettext t' textdomain str str_plural n =
  t' false (Some textdomain) str (Some (str_plural,n)) LC_MESSAGES
;;

let fdngettext t' textdomain str str_plural n =
  format_of_string (t' true (Some textdomain) str (Some (str_plural,n)) LC_MESSAGES)
;;

let dcngettext t' textdomain str str_plural n category = 
  t' false (Some textdomain) str (Some(str_plural,n)) category
;;
  
let fdcngettext t' textdomain str str_plural n category =
  format_of_string (t' true (Some textdomain) str (Some(str_plural,n)) category)
;;
