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

include ../ConfMakefile

all:

ifeq ($(BUILD_DOC),yes)
# HTML documentation
HTML_TARGETS=$(call HTML_TARGET,reference-manual.xml)
$(call HTML_TARGET,reference-manual.xml): IMAGES=gui.png gui-fr.png

# PDF documentation
PDF_TARGETS=$(call PDF_TARGET,reference-manual.xml)

ODOC_TARGETS=$(call ODOC_TARGET,gettext) $(call ODOC_TARGET,gettext-ext)

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

$(call ODOC_TARGET,gettext):REQUIRES=$(ALL_PACKAGES)
$(call ODOC_TARGET,gettext):OCAMLDOCFLAGS+=-css-style style.css
$(call ODOC_TARGET,gettext):EXTRAFILES+=style.css
$(call ODOC_TARGET,gettext):OCAMLDOCFLAGS+=-I ../libgettext-ocaml/
$(call ODOC_TARGET,gettext):../libgettext-ocaml/gettext.mli 
$(call ODOC_TARGET,gettext):../libgettext-ocaml/gettextTypes.ml

ifeq ($(BUILD_CAMOMILE),yes)
$(call ODOC_TARGET,gettext):../libgettext-camomile-ocaml/gettextCamomile.ml
$(call ODOC_TARGET,gettext):OCAMLDOCFLAGS+=-I ../libgettext-camomile-ocaml/
endif

ifeq ($(BUILD_STUB),yes)
$(call ODOC_TARGET,gettext):../libgettext-stub-ocaml/gettextStub.ml
$(call ODOC_TARGET,gettext):OCAMLDOCFLAGS+=-I ../libgettext-stub-ocaml/
endif

$(call ODOC_TARGET,gettext-ext):OCAMLDOCFLAGS+=-I ../libgettext-ocaml 
$(call ODOC_TARGET,gettext-ext):REQUIRES=$(ALL_PACKAGES)
$(call ODOC_TARGET,gettext-ext):OCAMLDOCFLAGS+=-css-style style.css
$(call ODOC_TARGET,gettext-ext):EXTRAFILES+=style.css
$(call ODOC_TARGET,gettext-ext):$(wildcard ../libgettext-ocaml/*.ml)
$(call ODOC_TARGET,gettext-ext):$(wildcard ../libgettext-ocaml/*.mli)
endif
