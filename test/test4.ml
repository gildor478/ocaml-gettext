
let t' = ()
;;

let _ = d_ "d_"
;;

let _ = f_ "f_"
;;

let _ = sn_ "sn_ singular" "sn_ plural" 0 
;;

let _ = fn_ "fn_ singular" "fn_ plural" 0
;;

let _ = gettext t' "gettext"
;;

let _ = fgettext t' "fgettext" 
;;

let _ = dgettext t' "mydomain" "dgettext"
;;

let _ = fdgettext t' "mydomain"  "fdgettext"
;;

let _ = dcgettext t'  "mydomain"  "dcgettext"  LC_ALL
;;

let _ = fdcgettext t'  "mydomain"  "fdcgettext"  LC_ALL
;;

let _ = ngettext t'  "ngettext singular"  "ngettext plural"  0 
;;

let _ = fngettext t'  "fngettext singular"  "fngettext plural"  0 
;;

let _ = dngettext t'  "mydomain"  "dngettext singular"  "dngettext plural "  0 
;;

let _ = fdngettext t'  "mydomain"  "fdngettext singular"  "fdngettext plural"  0 
;;

let _ = dcngettext t'  "mydomain"  "dcngettext singular"  "dcngettext plural"  0  LC_ALL 
;;

let _ = fdcngettext t'  "mydomain"  "fdcngettext singular"  "fdcngettext plural"  0  LC_ALL 
;;

let _ = TestGettext.d_ "TestGettext.d_"
;;

let _ = TestGettext.f_ "TestGettext.f_"
;;

let _ = TestGettext.sn_ "TestGettext.sn_ singular" "TestGettext.sn_ plural" 0 
;;

let _ = TestGettext.fn_ "TestGettext.fn_ singular" "TestGettext.fn_ plural" 0
;;

let _ = GettextCompat.gettext t' "GettextCompat.gettext"
;;

let _ = GettextCompat.fgettext t' "GettextCompat.fgettext" 
;;

let _ = GettextCompat.dgettext t' "mydomain" "GettextCompat.dgettext"
;;

let _ = GettextCompat.fdgettext t' "mydomain"  "GettextCompat.fdgettext"
;;

let _ = GettextCompat.dcgettext t'  "mydomain"  "GettextCompat.dcgettext"  LC_ALL
;;

let _ = GettextCompat.fdcgettext t'  "mydomain"  "GettextCompat.fdcgettext"  LC_ALL
;;

let _ = GettextCompat.ngettext t'  "GettextCompat.ngettext singular"  "GettextCompat.ngettext plural"  0 
;;

let _ = GettextCompat.fngettext t'  "GettextCompat.fngettext singular"  "GettextCompat.fngettext plural"  0 
;;

let _ = GettextCompat.dngettext t'  "mydomain"  "GettextCompat.dngettext singular"  "GettextCompat.dngettext plural "  0 
;;

let _ = GettextCompat.fdngettext t'  "mydomain"  "GettextCompat.fdngettext singular"  "GettextCompat.fdngettext plural"  0 
;;

let _ = GettextCompat.dcngettext t'  "mydomain"  "GettextCompat.dcngettext singular"  "GettextCompat.dcngettext plural"  0  LC_ALL 
;;

let _ = GettextCompat.fdcngettext t'  "mydomain"  "GettextCompat.fdcngettext singular"  "GettextCompat.fdcngettext plural"  0  LC_ALL 
;;

let _ = TestGettext.Library.d_ "TestGettext.Library.d_"
;;

let _ = TestGettext.Library.f_ "TestGettext.Library.f_"
;;

let _ = TestGettext.Library.sn_ "TestGettext.Library.sn_ singular" "TestGettext.Library.sn_ plural" 0 
;;

let _ = TestGettext.Library.fn_ "TestGettext.Library.fn_ singular" "TestGettext.Library.fn_ plural" 0
;;

let _ = TestGettext.Program.d_ "TestGettext.Program.d_"
;;

let _ = TestGettext.Program.f_ "TestGettext.Program.f_"
;;

let _ = TestGettext.Program.sn_ "TestGettext.Program.sn_ singular" "TestGettext.Program.sn_ plural" 0 
;;

let _ = TestGettext.Gettext.Program.fn_ "TestGettext.Program.fn_ singular" "TestGettext.Program.fn_ plural" 0
;;

