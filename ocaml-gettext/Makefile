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

#################################
# Ocaml Gettext utility program #
#################################

NAME       = ocaml-gettext
CMO        = ocaml-gettext.cmo      
REQUIRES   = gettext.extension fileutils

REQUIRES  += gettext-camomile

include ../TopMakefile

#################################
# Ocaml Gettext extract program #
#################################

all: ocaml-xgettext

install: ocaml-xgettext-install
	
uninstall: ocaml-xgettext-uninstall
	
ocaml-xgettext: $(BUILDBIN)
	$(MKCAMLP4)                                                          \
           -I +camlp4 pa_o.cmo                                               \
          `$(OCAMLFIND) query -r -predicates byte gettext.extract -i-format` \
          `$(OCAMLFIND) query -r -predicates byte gettext.extract -a-format` \
          `$(OCAMLFIND) query -r -predicates byte gettext.extract -o-format` \
          -o $@
	$(CP) $@ $(BUILDBIN)/


ocaml-xgettext-install:
	$(CP) ocaml-xgettext $(BINDIR)

ocaml-xgettext-uninstall:
	-$(RM) $(BINDIR)/ocaml-xgettext 

clean:: 
	-$(RM) ocaml-xgettext
	-$(RM) $(BUILDBIN)/ocaml-xgettext
	-$(if $(wildcard $(BUILDBIN)/*), ,$(RM) -r $(BUILDBIN))
	-$(if $(wildcard $(BUILDDIR)/*), ,$(RM) -r $(BUILDDIR))

