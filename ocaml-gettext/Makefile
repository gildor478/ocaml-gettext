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

include ../ConfMakefile

ifeq ($(BUILD_CAMOMILE),yes)
GETTEXT_MODULES=CAMOMILE
else
ifeq ($(BUILD_STUB),yes)
GETTEXT_MODULES=STUB
endif
endif

ifdef GETTEXT_MODULES
#################################
# Ocaml Gettext utility program #
#################################

NAME       = ocaml-gettext
CMO        = OCamlGettext.cmo      
REQUIRES   = gettext.extension fileutils

ifeq ($(GETTEXT_MODULES),CAMOMILE)
REQUIRES  += gettext-camomile
INCLUDES  += -pp 'camlp4o -I +camlp4 pa_macro.cmo -DCAMOMILE'
endif

ifeq ($(GETTEXT_MODULES),STUB)
REQUIRES  += gettext-stub
INCLUDES  += -pp 'camlp4o -I +camlp4 pa_macro.cmo -DSTUB'
endif

include ../TopMakefile

#################################
# Ocaml Gettext extract program #
#################################

all: ocaml-xgettext

install: ocaml-xgettext-install

uninstall: ocaml-xgettext-uninstall

ocaml-xgettext: $(BUILDBIN)
	$(OCAMLC)                                                            \
           -I +camlp4 dynlink.cma camlp4lib.cma                              \
           unix.cma                                                          \
          `$(OCAMLFIND) query -r -predicates byte gettext.extract -i-format` \
          `$(OCAMLFIND) query -r -predicates byte gettext.extract -a-format` \
          `$(OCAMLFIND) query -r -predicates byte gettext.extract -o-format` \
           Camlp4Bin.cmo                                                     \
          -o $@
	$(INSTALL_SCRIPT) -t $(BUILDBIN) $@

ocaml-xgettext-install:
	$(INSTALL_SCRIPT) -t $(BINDIR) ocaml-xgettext 

ocaml-xgettext-uninstall:
	-$(RM) $(BINDIR)/ocaml-xgettext 

clean:: 
	-$(RM) ocaml-xgettext
	-$(RM) $(BUILDBIN)/ocaml-xgettext
endif
