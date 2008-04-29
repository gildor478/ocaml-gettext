##########################################################################
#  ocaml-gettext: a library to translate messages                        #
#                                                                        #
#  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         #
#                                                                        #
#  This library is free software; you can redistribute it and/or         #
#  modify it under the terms of the GNU Lesser General Public            #
#  License as published by the Free Software Foundation; either          #
#  version 2.1 of the License, or (at your option) any later version;    #
#  with the OCaml static compilation exception.                          #
#                                                                        #
#  This library is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  Lesser General Public License for more details.                       #
#                                                                        #
#  You should have received a copy of the GNU Lesser General Public      #
#  License along with this library; if not, write to the Free Software   #
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   #
#  USA                                                                   #
##########################################################################

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
  $3
fi
])


# AC_CHECK_OCAMLC([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamlc (or ocamlc.opt)
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
    AC_MSG_WARN($versionval doesn't match ocaml v. $OCAMLVERSION)
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
# Check for the existence of ocamlopt (or ocamlopt.opt)
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
    AC_MSG_WARN($versionval doesn't match ocaml v. $OCAMLVERSION)
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
# Check for the existence of ocamllex (or ocamllex.opt)
# Subst OCAMLLEX, OCAMLVERSION variable.
AC_DEFUN([AC_CHECK_OCAMLLEX],
[
dnl Check the presence of ocamlc/ocamlc.opt and their version
AC_PROG_OCAML_VERSION(ocamllex,ocamllex.opt,[
  OCAMLLEX=$toolval
  if ! test "x$OCAMLVERSION" = "x" && ! test "$versionval" = "$OCAMLVERSION"; then
    AC_MSG_WARN($versionval doesn't match ocaml v. $OCAMLVERSION)
  else
    OCAMLVERSION=$versionval
  fi
],[$2])
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLVERSION)
])

# AC_CHECK_OCAMLYACC([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamlyacc (or ocamlyacc.opt)
# Subst OCAMLYACC variable.
AC_DEFUN([AC_CHECK_OCAMLYACC],
[
dnl Check the presence of ocamlyacc/ocamlyacc.opt and their version
AC_CHECK_PROG(ac_ocaml_ocamlyacc,ocamlyacc,ocamlyacc)
AC_CHECK_PROG(ac_ocaml_ocamlyacc_opt,ocamlyacc.opt,ocamlyacc.opt)
if test "x$ac_ocaml_ocamlyacc" = "x" && test "x$ac_ocaml_ocamlyacc_opt" = "x"; then
  :
  $2
elif ! test "x$ac_ocaml_ocamlyacc_opt" = "x"; then
  OCAMLYACC=$ac_ocaml_ocamlyacc_opt
  $1
else
  OCAMLYACC=$ac_ocaml_ocamlyacc
  $1
fi
AC_SUBST(OCAMLYACC)
])

# AC_CHECK_OCAMLBUILD([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamlbuild.
# Subst OCAMLBUIKD variable.
AC_DEFUN([AC_CHECK_OCAMLBUILD],
[
dnl Check the presence of ocamlbuild/ocamlbuild.opt and their version
AC_CHECK_PROG(ac_ocaml_ocamlbuild,ocamlbuild,ocamlbuild)
if test "x$ac_ocaml_ocamlbuild" = "x"; then
  :
  $2
else
  OCAMLBUILD=$ac_ocaml_ocamlbuild
  $1
fi
AC_SUBST(OCAMLBUILD)
])


# AC_CHECK_OCAMLFIND ([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of ocamlfind.
# Subst OCAMLFIND variable.
AC_DEFUN([AC_CHECK_OCAMLFIND],
[
dnl Check the presence of ocamlfind
AC_CHECK_PROG(ac_ocaml_ocamlfind,ocamlfind,ocamlfind)
if test "x$ac_ocaml_ocamlfind" = "x"; then
  :
  $2
else
  :
  OCAMLFIND=$ac_ocaml_ocamlfind
  $1
fi
AC_SUBST(OCAMLFIND)
])

# OCAMLFIND_CHECK_MODULE (MODULE,[ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Check for the existence of the module using OCAMLFIND
AC_DEFUN([OCAMLFIND_CHECK_MODULE],
[
AC_REQUIRE([AC_CHECK_OCAMLFIND])
dnl Check the presence of module $1
AC_MSG_CHECKING(for module $1)
if ! test "x$OCAMLFIND" = "x" ; then
  ac_ocaml_pkg_$1=`$OCAMLFIND query $1 2> /dev/null`
  if ! test "x$ac_ocaml_pkg_$1" = "x"; then
    AC_MSG_RESULT($ac_ocaml_pkg_$1)
    $2
  else
    AC_MSG_RESULT(no)
    $3
  fi
else
  AC_MSG_RESULT(no)
  $3
fi
])

# AC_CHECK_[CAMLP4,CAMLIDL,OCAMLMKLIB,MKCAMLP4] ([ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#---------------------------------------------------------
# Subst the corresponding var
AC_DEFUN([AC_CHECK_STAR],
[
AC_CHECK_PROG($1,$2,$2)
if test "x$$1" = "x"; then
  :
  $4
else
  :
  $3
fi
AC_SUBST($1)
])
AC_DEFUN([AC_CHECK_CAMLP4],     [AC_CHECK_STAR(CAMLP4,camlp4,$1,$2)])
AC_DEFUN([AC_CHECK_CAMLP4OF],   [AC_CHECK_STAR(CAMLP4OF,camlp4of,$1,$2)])
AC_DEFUN([AC_CHECK_CAMLP4O],    [AC_CHECK_STAR(CAMLP4O,camlp4o,$1,$2)])
AC_DEFUN([AC_CHECK_CAMLIDL],    [AC_CHECK_STAR(CAMLIDL,camlidl,$1,$2)])
AC_DEFUN([AC_CHECK_OCAMLMKLIB], [AC_CHECK_STAR(OCAMLMKLIB,ocamlmklib,$1,$2)])
AC_DEFUN([AC_CHECK_MKCAMLP4],   [AC_CHECK_STAR(MKCAMLP4,mkcamlp4,$1,$2)])
AC_DEFUN([AC_CHECK_OCAMLDOC],   [AC_CHECK_STAR(OCAMLDOC,ocamldoc,$1,$2)])
AC_DEFUN([AC_CHECK_XSLTPROC],   [AC_CHECK_STAR(XSLTPROC,xsltproc,$1,$2)])
AC_DEFUN([AC_CHECK_XMLLINT],    [AC_CHECK_STAR(XMLLINT,xmllint,$1,$2)])

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

# AC_CHECK_XSL (VAR,config,DEFAULT_STYLESHEET,[ACTION-IF-OK],[ACTION-IF-NOT-OK])
#---------------------------------------------------------
# Check the presence and the possibility to validate
# and apply the given stylesheet
# Subst XSLTPROC, XMLLINT, DOCBOOK_STYLESHEET_VAR,
AC_DEFUN([AC_CHECK_XSL],
[
AC_REQUIRE([AC_CHECK_XSLTPROC])
AC_REQUIRE([AC_CHECK_XMLLINT])

AC_ARG_WITH(docbook-stylesheet-$2,
        AC_HELP_STRING([--with-docbook-stylesheet-$2=file], [Where to find the docbook stylesheet for $2 generation]),
        $1=$withval, $1="$3")
        
AC_MSG_CHECKING(for $2 XSL)
if ! test -e "$$1"; then
  AC_MSG_RESULT(no)
  $1=
else
  AC_MSG_RESULT($$1)
fi

if ! test "x$$1" = "x" && ! test "x$XSLTPROC" = "x"; then
  :
  $4
else
  :
  $5
fi

AC_SUBST($1)
])


# AC_CHECK_HTMLXSL (DEFAULT_STYLESHEET,[ACTION-IF-OK],[ACTION-IF-NOT-OK])
#---------------------------------------------------------
# Check the possibility to generate HTML out of Docbook XML
AC_DEFUN([AC_CHECK_HTMLXSL],
[
AC_CHECK_XSL(HTMLXSL,html,$1,$2,$3)
]);

# AC_CHECK_MANXSL (STYLESHEET,DEFAULT_STYLESHEET,[ACTION-IF-OK],[ACTION-IF-NOT-OK])
#---------------------------------------------------------
# Check the possibility to generate manpages out of Docbook XML
AC_DEFUN([AC_CHECK_MANXSL],
[
AC_CHECK_XSL(MANXSL,manpages,$1,$2,$3)
]);

# AC_CHECK_PDFXSL (STYLESHEET,DEFAULT_STYLESHEET,[ACTION-IF-OK],[ACTION-IF-NOT-OK])
#---------------------------------------------------------
# Check the possibility to generate PDF out of Docbook XML
AC_DEFUN([AC_CHECK_PDFXSL],
[
AC_CHECK_XSL(FOXSL,pdf,$1,$2,$3)
AC_CHECK_PROG(FOP,fop,fop)
if test "x$FOP" = "x"; then
  AC_MSG_WARN(Cannot find fop.)
  $3
fi
]);

