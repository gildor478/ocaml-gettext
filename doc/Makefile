HTML_SOURCES=reference-manual.xml
MAN_SOURCES=ocaml-gettext.xml ocaml-xgettext.xml ocaml-gettext-options.xml

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
	$(XSLTPROC) --xinclude --stringparam base.dir        \
	  "$(dir $@)" $(HTMLXSL) $^

install-builddoc-html: $(patsubst %.xml,$(BUILDHTML)/%/index.html,$(HTML_SOURCES))

clean-doc-html: 
	-$(RM) -r $(patsubst %.xml,$(BUILDHTML)/%,$(HTML_SOURCES))
	
install-doc-html: install-builddoc-html
	$(CP) -r $(patsubst %.xml,$(BUILDHTML)/%,$(HTML_SOURCES)) $(DOCHTML)

uninstall-doc-html: clean-doc-html
	-$(RM) -r $(patsubst %.xml,$(DOCHTML)/%,$(HTML_SOURCES))

# Build MAN files

install-builddoc-man: $(MAN_SOURCES)
	for i in $^; do                                 \
	  $(XMLLINT) --xinclude --noent --noout         \
	    --postvalid $$i;                            \
	  $(XSLTPROC) --xinclude                        \
	    $(MANXSL) $$i;                              \
	done

clean-doc-man:
	-$(RM) $(MAN_FILES)

install-doc-man: BUILDMAN=$(DOCMAN)
install-doc-man: install-builddoc-man

uninstall-doc-man: BUILDMAN=$(DOCMAN)
uninstall-doc-man: clean-doc-man
