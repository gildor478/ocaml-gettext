(* Create the module Gettext, using the textdomain "mydomain" *)
module Gettext = Gettext.Program(struct
  let textdomain   = "mydomain"
  let codeset      = None
  let dir          = None
  let dependencies = Library.init
  (* Since, we use a GUI program that uses GTK translation functions,
     we use GettextStub.Native which give use the same translation
     layer as GTK 
   *)
  let realize      = GettextStub.Native.realize
end)
;;

(* Create shortand to call Gettext functions. It is important to 
   keep the name s_, f_, sn_ and fn_ *)
let s_ = Gettext.s_
;;

let f_ = Gettext.f_
;;

let sn_ = Gettext.sn_
;;

let fn_ = Gettext.fn_
;;
