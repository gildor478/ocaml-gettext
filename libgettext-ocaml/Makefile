##########################################################################
#  Ocaml-gettext : a library to translate messages                       #
#                                                                        #
#  Copyright (C) 2003, 2004, 2005 Sylvain Le Gall <sylvain@le-gall.net>  #
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
#                                                                        #
#  Contact: sylvain@le-gall.net                                          #
##########################################################################

NAME       = gettext
CMO        =       
LIBRARY    = true
REQUIRES   = fileutils
PREDICATES =
INSTALLIB  = META

all:

distclean::
	-$(RM) gettextConfig.ml
	-$(RM) META

##############################
# Ocaml Gettext base library #
##############################

INSTALLIB +=              \
 gettextBase.cma          \
 gettextBase.cmxa         \
 gettextBase.a            \
 gettextTypes.cmi         \
 gettextCompat.cmi        \
 gettext.cmi         

CMO_BASE  =               \
 gettextConfig.cmo        \
 gettextTypes.cmo         \
 gettextUtils.cmo         \
 gettextModules.cmo       \
 gettextCompat.cmo        \
 gettext.cmo              \
 gettextFormat_parser.cmo \
 gettextFormat_lexer.cmo  \
 gettextFormat.cmo        \
 gettextMo_int32.cmo      \
 gettextMo_parser.cmo     \
 gettextMo_lexer.cmo      \
 gettextMo.cmo            

gettextCompat.cmo: gettextCompat.cmi 
gettextCompat.cmx: gettextCompat.cmi 
gettext.cmo: gettext.cmi 
gettext.cmx: gettext.cmi 
gettextMo_parser.cmo: gettextMo_parser.cmi
gettextMo_parser.cmx: gettextMo_parser.cmi
gettextFormat_parser.cmo: gettextFormat_parser.cmi
gettextFormat_parser.cmx: gettextFormat_parser.cmi
gettextBase.cma: $(CMO_BASE)
gettextBase.cmxa: $(CMO_BASE:.cmo=.cmx)

clean::
	-$(RM) gettextMo_parser.output \
	 gettextMo_parser.ml gettextMo_parser.mli

###################################
# Ocaml Gettext extension library #
###################################

INSTALLIB +=           \
 gettextExtension.cma  \
 gettextExtension.cmxa \
 gettextExtension.a    \
 gettextConfig.cmi     \
 gettextModules.cmi    \
 gettextFormat.cmi     \
 gettextMo.cmi         \
 gettextCategory.cmi   \
 gettextLocale.cmi     \
 gettextDomain.cmi     \
 gettextCharset.cmi    \
 gettextTranslate.cmi  \
 gettextRealize.cmi    \
 gettextPo.cmi         \
 gettextUtils.cmi      \
 gettextCompile.cmi         

CMO_EXTENSION=            \
 gettextCategory.cmo      \
 gettextLocale_types.cmo  \
 gettextLocale_parser.cmo \
 gettextLocale_lexer.cmo  \
 gettextLocale.cmo        \
 gettextDomain.cmo        \
 gettextCharset.cmo       \
 gettextTranslate.cmo     \
 gettextRealize.cmo       \
 gettextPo_utils.cmo      \
 gettextPo_parser.cmo     \
 gettextPo_lexer.cmo      \
 gettextPo.cmo            \
 gettextCompile.cmo

gettextPo_parser.cmo: gettextPo_parser.cmi
gettextPo_parser.cmx: gettextPo_parser.cmi
gettextLocale_parser.cmo: gettextLocale_parser.cmi
gettextLocale_parser.cmx: gettextLocale_parser.cmi
gettextExtension.cma: $(CMO_EXTENSION)
gettextExtension.cmxa: $(CMO_EXTENSION:.cmo=.cmx)

clean::
	-$(RM) gettextPo_parser.output \
	 gettextPo_parser.ml gettextPo_parser.mli

########################
# Pa_gettext extension #
########################

INSTALLIB +=        \
 pr_gettext.cmo    

pr_gettext.cmo: REQUIRES=ast-analyze

include ../ConfMakefile
include ../TopMakefile
