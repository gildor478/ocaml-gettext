(** Concrete implementation based on camomile 
    ( @see <http://camomile.sourceforge.net/> Camomile library ) 
*)

open Camomile;;
open GettextTypes;;

module Charset : GettextCharset.CHARSET_TYPE =
  struct
    type encoding = string
    type u = {
      failsafe : failsafe;
      in_enc   : CharEncoding.t;
      out_enc  : CharEncoding.t;
    }

    let create t in_enc out_enc = {
      failsafe = t.GettextTypes.failsafe;
      in_enc   = CharEncoding.of_name in_enc;
      out_enc  = CharEncoding.of_name out_enc;
    }

    let recode u str = 
      CharEncoding.recode_string u.in_enc u.out_enc str
  end
;;

module Map : GettextRealize.REALIZE_TYPE =
 GettextRealize.Generic 
 (GettextTranslate.Map)     (* Map translation *)
 (Charset)                  (* Camomile charset *)
 (GettextLocale.Posix)      (* POSIX locale *)
;;
 
module Hashtbl : GettextRealize.REALIZE_TYPE =
 GettextRealize.Generic 
 (GettextTranslate.Hashtbl) (* Hashtbl translation *)
 (Charset)                  (* Camomile charset *)
 (GettextLocale.Posix)      (* POSIX locale *)
;;
 
module Open : GettextRealize.REALIZE_TYPE =
 GettextRealize.Generic 
 (GettextTranslate.Open)    (* Open translation *)
 (Charset)                  (* Camomile charset *)
 (GettextLocale.Posix)      (* POSIX locale *)
;;
  
