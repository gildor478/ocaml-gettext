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

all:

ifeq ($(BUILD_DOC),yes)
# HTML documentation
HTML_TARGETS=$(call HTML_TARGET,reference-manual.xml)
$(call HTML_TARGET,reference-manual.xml): IMAGES=gui.png gui-fr.png

# PDF documentation
ifeq ($(BUILD_DOC_PDF),yes)
PDF_TARGETS=$(call PDF_TARGET,reference-manual.xml)
endif

ODOC_TARGETS=$(call ODOC_TARGET,api) $(call ODOC_TARGET,api-ext)

# Manpage documentation
MAN_TARGETS=ocaml-gettext.1 ocaml-xgettext.1 ocaml-gettext.5
ocaml-gettext.1: ocaml-gettext.xml
ocaml-xgettext.1: ocaml-xgettext.xml
ocaml-gettext.5: ocaml-gettext-options.xml
endif

include ../TopMakefile

ifeq ($(BUILD_DOC),yes)
ALL_PACKAGES=gettext.base gettext.extension gettext.extract 

ifeq ($(BUILD_CAMOMILE),yes)
ALL_PACKAGES+=gettext-camomile 
endif

ifeq ($(BUILD_STUB),yes)
ALL_PACKAGES+=gettext-stub
endif

$(call ODOC_TARGET,api):REQUIRES=$(ALL_PACKAGES)
$(call ODOC_TARGET,api):OCAMLDOCFLAGS+=-I ../libgettext-ocaml/
$(call ODOC_TARGET,api):../libgettext-ocaml/gettext.mli 
$(call ODOC_TARGET,api):../libgettext-ocaml/gettextTypes.ml
$(call ODOC_TARGET,api):../libgettext-ocaml/gettextDummy.ml

ifeq ($(BUILD_CAMOMILE),yes)
$(call ODOC_TARGET,api):../libgettext-camomile-ocaml/gettextCamomile.mli
$(call ODOC_TARGET,api):OCAMLDOCFLAGS+=-I ../libgettext-camomile-ocaml/
endif

ifeq ($(BUILD_STUB),yes)
$(call ODOC_TARGET,api):../libgettext-stub-ocaml/gettextStub.ml
$(call ODOC_TARGET,api):OCAMLDOCFLAGS+=-I ../libgettext-stub-ocaml/
endif

$(call ODOC_TARGET,api-ext):OCAMLDOCFLAGS+=-I ../libgettext-ocaml 
$(call ODOC_TARGET,api-ext):REQUIRES=$(ALL_PACKAGES)
$(call ODOC_TARGET,api-ext):$(filter-out ../libgettext-ocaml/pr_gettext.ml,$(wildcard ../libgettext-ocaml/*.ml))
$(call ODOC_TARGET,api-ext):$(wildcard ../libgettext-ocaml/*.mli)
endif

clean::
	-$(RM) '$(CURDIR)/ocaml-gettext.5'
	-$(RM) '$(CURDIR)/ocaml-xgettext.1'
	-$(RM) '$(CURDIR)/ocaml-gettext.1'
