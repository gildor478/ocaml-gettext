all:

HTML_SOURCES=reference-manual.xml

# Manpages are a little bit more complicated
MAN_SOURCES=ocaml-gettext.xml ocaml-xgettext.xml ocaml-gettext-options.xml
MAN_TARGETS=ocaml-gettext.1   ocaml-xgettext.1   ocaml-gettext-options.5
ocaml-gettext.1: ocaml-gettext.xml
ocaml-xgettext.1: ocaml-xgettext.xml
ocaml-gettext-options.5: ocaml-gettext-options.xml

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


all: install-builddoc-html install-builddoc-man

install: install-doc-html install-doc-man

uninstall: uninstall-doc-html uninstall-doc-man

clean: clean-doc-html clean-doc-man

# Build HTML files

$(BUILDHTML)/%/index.html: %.xml
	$(XMLLINT) --xinclude --noent --noout --postvalid $^ 
	$(MKDIR) -p $(dir $@)
	$(XSLTPROC) --xinclude --stringparam base.dir "$(dir $@)" $(HTMLXSL) $^

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

%.1 %.2 %.3 %.4 %.5 %.6 %.7 %.8 %.9: 
	$(XMLLINT) --xinclude --noent --noout --postvalid $^
	$(XSLTPROC) --xinclude $(MANXSL) $^
	$(MKDIR) -p $(dir $(call MAN_BUILD_TARGETS,$@))
	$(CP) $@ $(call MAN_BUILD_TARGETS,$@) 
	
install-builddoc-man: $(MAN_TARGETS)

clean-doc-man:
	-$(RM) $(MAN_TARGETS) $(foreach i,$(MAN_TARGETS),$(call MAN_BUILD_TARGETS,$(i)))

install-doc-man: install-builddoc-man
	$(CP) -r $(BUILDMAN) $(DOCMAN)

uninstall-doc-man: clean-doc-man
	-$(RM) $(foreach i,$(MAN_TARGETS),$(call MAN_INSTALL_TARGETS,$(i)))

