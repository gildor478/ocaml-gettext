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

all:

HTML_SOURCES=reference-manual.xml
PDF_SOURCES=reference-manual.xml
ODOC_TARGETS=api api-extended

# Manpages are a little bit more complicated
MAN_TARGETS=ocaml-gettext.1 ocaml-xgettext.1 ocaml-gettext.5
ocaml-gettext.1: ocaml-gettext.xml
ocaml-xgettext.1: ocaml-xgettext.xml
ocaml-gettext.5: ocaml-gettext-options.xml

# Destination dir
DOCHTML=
DOCMAN=

BUILDHTML=html
BUILDMAN=man

# Programs
MKDIR=mkdir
XSLTPROC=xsltproc
XMLLINT=xmllint
CP=cp

# Stylesheets
HTMLXSL=/usr/share/xml/docbook/stylesheet/nwalsh/xhtml/chunk.xsl
MANXSL=/usr/share/xml/docbook/stylesheet/nwalsh/manpages/docbook.xsl
PDFXSL=

# Not at the right place 
$(BUILDHTML)/reference-manual/index.html: IMAGES=gui.png gui-fr.png
	
all: install-builddoc-html install-builddoc-man

install: install-doc-html install-doc-man

uninstall: uninstall-doc-html uninstall-doc-man

clean: clean-doc-html clean-doc-man
	$(RM) -r $(BUILDHTML) $(BUILDMAN)

# Build HTML files

$(BUILDHTML)/%/index.html: %.xml
	$(XMLLINT) --xinclude --noent --noout --postvalid $^ 
	$(MKDIR) -p $(dir $@)
	$(XSLTPROC) --xinclude --stringparam base.dir "$(dir $@)" $(HTMLXSL) $^
	$(if $(IMAGES),$(CP) $(IMAGES) $(dir $@))

install-builddoc-html: $(patsubst %.xml,$(BUILDHTML)/%/index.html,$(HTML_SOURCES))

clean-doc-html: 
	-$(RM) -r $(patsubst %.xml,$(BUILDHTML)/%,$(HTML_SOURCES))
	
install-doc-html: install-builddoc-html
	$(CP) -r $(patsubst %.xml,$(BUILDHTML)/%,$(HTML_SOURCES)) $(DOCHTML)

uninstall-doc-html: clean-doc-html
	-$(RM) -r $(patsubst %.xml,$(DOCHTML)/%,$(HTML_SOURCES))

# Build MAN files

MAN_SECTION=man$(patsubst .%,%,$(suffix $(1)))
MAN_BUILD_TARGETS=$(BUILDMAN)/$(call MAN_SECTION,$(1))/$(1)
MAN_INSTALL_TARGETS=$(DOCMAN)/$(call MAN_SECTION,$(1))/$(1)

define PROCESS_MAN
	$(XMLLINT) --xinclude --noent --noout --postvalid $^
	$(XSLTPROC) --xinclude $(MANXSL) $^
	$(MKDIR) -p $(dir $(call MAN_BUILD_TARGETS,$@))
	$(CP) $@ $(call MAN_BUILD_TARGETS,$@) 
endef

%.1:
	$(PROCESS_MAN)
%.2:
	$(PROCESS_MAN)
%.3:
	$(PROCESS_MAN)
%.4:
	$(PROCESS_MAN)
%.5:
	$(PROCESS_MAN)
%.6:
	$(PROCESS_MAN)
%.7:
	$(PROCESS_MAN)
%.8:
	$(PROCESS_MAN)
%.9:
	$(PROCESS_MAN)
	
install-builddoc-man: $(MAN_TARGETS)

clean-doc-man:
	-$(RM) $(MAN_TARGETS) $(foreach i,$(MAN_TARGETS),$(call MAN_BUILD_TARGETS,$(i)))

install-doc-man: install-builddoc-man
	$(CP) -r $(BUILDMAN) $(DOCMAN)

uninstall-doc-man: clean-doc-man
	-$(RM) $(foreach i,$(MAN_TARGETS),$(call MAN_INSTALL_TARGETS,$(i)))

