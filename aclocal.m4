# AC_PROG_OCAML_VERSION(prog,prog.opt,[ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of an ocaml tool and parse its version
# Version in $versionval, tool in $toolval

AC_DEFUN([AC_PROG_OCAML_VERSION],
[
AC_CHECK_PROG(ac_ocaml_$1,$1,$1)
AC_CHECK_PROG(ac_ocaml_$1_opt,$2,$2)
if test "x$ac_ocaml_$1" = "x" && test "x$ac_ocaml_$1_opt" = "x"; then
  :
  $4
else 
  if ! test "x$ac_ocaml_$1" = "x"; then
    ac_ocaml_$1_version=`$ac_ocaml_$1 -v | sed -n -e "s|.*version *\(.*\)$|\1|p" `
    versionval=$ac_ocaml_$1_version
    toolval=$ac_ocaml_$1
  fi

  if ! test "x$ac_ocaml_$1_opt" = "x"; then
    ac_ocaml_$1_opt_version=`$ac_ocaml_$1 -v | sed -n -e "s|.*version *\(.*\)$|\1|p" `
    if ! test "x$ac_ocaml_$1" = "x" && test "$ac_ocaml_$1_opt_version" = "$ac_ocaml_$1_version"; then
      versionval=$ac_ocaml_$1_opt_version
      toolval=$ac_ocaml_$1_opt
    elif test "x$ac_ocaml_$1" = "x"; then
      versionval=$ac_ocaml_$1_opt_version
      toolval=$ac_ocaml_$1_opt
    else
      AC_MSG_WARN("$1 and $2 version differs")
    fi
  fi
fi
$3
])


# AC_CHECK_OCAMLC([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamlc ( or ocamlc.opt )
# Subst OCAMLC, OCAMLVERSION, OCAMLBEST variable.
AC_DEFUN([AC_CHECK_OCAMLC],
[
dnl Check the presence of ocamlc/ocamlc.opt and their version
AC_PROG_OCAML_VERSION(ocamlc,ocamlc.opt,[
  OCAMLC=$toolval
  if test "x$OCAMLBEST" = "x"; then
    OCAMLBEST=byte
  fi
  if ! test "x$OCAMLVERSION" = "x" && ! test "$versionval" = "$OCAMLVERSION"; then
    AC_MSG_WARN( $versionval doesn't match ocaml v. $OCAMLVERSION)
  else
    OCAMLVERSION=$versionval
  fi
],[$2])
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLBEST)
])

# AC_CHECK_OCAMLOPT([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamlopt ( or ocamlopt.opt )
# Subst OCAMLOPT, OCAMLVERSION, OCAMLBEST variable.
AC_DEFUN([AC_CHECK_OCAMLOPT],
[
dnl Check the presence of ocamlc/ocamlc.opt and their version
AC_PROG_OCAML_VERSION(ocamlopt,ocamlopt.opt,[
  OCAMLOPT=$toolval
  if test "x$OCAMLBEST" = "x" || test "$OCAMLBEST" = "byte"; then
    OCAMLBEST=opt
  fi
  if ! test "x$OCAMLVERSION" = "x" && ! test "$versionval" = "$OCAMLVERSION"; then
    AC_MSG_WARN( $versionval doesn't match ocaml v. $OCAMLVERSION)
  else
    OCAMLVERSION=$versionval
  fi
],[$2])
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLBEST)
])

# AC_CHECK_OCAMLLEX([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamllex ( or ocamllex.opt )
# Subst OCAMLLEX, OCAMLVERSION variable.
AC_DEFUN([AC_CHECK_OCAMLLEX],
[
dnl Check the presence of ocamlc/ocamlc.opt and their version
AC_PROG_OCAML_VERSION(ocamllex,ocamllex.opt,[
  OCAMLLEX=$toolval
  if ! test "x$OCAMLVERSION" = "x" && ! test "$versionval" = "$OCAMLVERSION"; then
    AC_MSG_WARN( $versionval doesn't match ocaml v. $OCAMLVERSION)
  else
    OCAMLVERSION=$versionval
  fi
],[$2])
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLVERSION)
])

# AC_CHECK_OCAMLYACC([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamlyacc ( or ocamlyacc.opt )
# Subst OCAMLYACC, OCAMLVERSION variable.
AC_DEFUN([AC_CHECK_OCAMLYACC],
[
dnl Check the presence of ocamlc/ocamlc.opt and their version
AC_CHECK_PROG(ac_ocaml_ocamlyacc,ocamlyacc,ocamlyacc)
AC_CHECK_PROG(ac_ocaml_ocamlyacc_opt,ocamlyacc.opt,ocamlyacc.opt)
if test "x$ac_ocaml_ocamlyacc" = "x" && test "x$ac_ocaml_ocamlyacc_opt" = "x"; then
  :
  $2
elif test "x$ac_ocaml_ocamlyacc_opt" = "x"; then
  OCAMLYACC=$ac_ocaml_ocamlyacc_opt
  $1
else
  OCAMLYACC=$ac_ocaml_ocamlyacc
  $1
fi
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLVERSION)
])

# AC_LIB_OCAML ()
#---------------------------------------------------------
# Get the library path
# Subst OCAMLLIB
AC_DEFUN([AC_LIB_OCAML],
[
AC_REQUIRE([AC_CHECK_OCAMLC])
AC_MSG_CHECKING(for ocaml libdir)
OCAMLLIB=`$OCAMLC -where`
AC_MSG_RESULT($OCAMLLIB)
AC_SUBST(OCAMLLIB)
])
