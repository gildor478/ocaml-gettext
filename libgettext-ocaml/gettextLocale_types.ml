
type locale = {
  language  : string;
  territory : string option;
  codeset   : string option;
  modifier  : string option;
}
;;

let create_locale language = {
  language  = language;
  territory = None;
  codeset   = None;
  modifier  = None;
}
;;
 
