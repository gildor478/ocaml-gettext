HTML_SOURCES=reference-manual.xml
MAN_SOURCES=ocaml-gettext.xml ocaml-xgettext.xml ocaml-gettext-options.xml

# Destination dir
BUILDHTML=html
BUILDMAN=man

# Programs
MKDIR=mkdir
XSLTPROC=xsltproc
TRUE=true
CP=cp

# Stylesheets
HTMLXSL=/usr/share/xml/docbook/stylesheet/nwalsh/xhtml/chunk.xsl
MANXSL=/usr/share/xml/docbook/stylesheet/nwalsh/manpages/docbook.xsl


all: install-builddoc-html install-builddoc-man

install: install-doc-html install-doc-man

uninstall: uninstall-doc-html uninstall-doc-man

clean: clean-doc-html clean-doc-man

# Build HTML files

install-builddoc-html: $(HTML_SOURCES)
	for i in $^; do                          \
	  DST=$(BUILDHTML)/$${i%%.xml}/;         \
	  $(MKDIR) -p $$DST;                     \
	  $(XSLTPROC) --xinclude                 \
	    --stringparam base.dir "$$DST"       \
	    $(HTMLXSL) $$i;                      \
	done

clean-doc-html: 
	-for i in $(HTML_SOURCES); do                   \
	  $(RM) -r $(BUILDHTML)/$${i%%.xml} || $(TRUE); \
	done

install-doc-html: BUILDHTML=$(DOCHTML)
install-doc-html: install-builddoc-html

uninstall-doc-html: BUILDHTML=$(DOCHTML)
uninstall-doc-html: clean-doc-html

# Build MAN files

install-builddoc-man: $(MAN_SOURCES)
	for i in $^; do                  \
	  $(XSLTPROC) --xinclude         \
	    $(MANXSL) $$i;               \
	done

clean-doc-man:
	-$(RM) $(MAN_FILES)

install-doc-man: BUILDMAN=$(DOCMAN)
install-doc-man: install-builddoc-man

uninstall-doc-man: BUILDMAN=$(DOCMAN)
uninstall-doc-man: clean-doc-man
