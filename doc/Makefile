BUILDHTML=html
MKDIR=mkdir
XSLTPROC=xsltproc --nonet --novalid
HTMLXSL=/usr/share/xml/docbook/stylesheet/nwalsh/xhtml/chunk.xsl

SOURCES=reference-manual.xml

all: install-builddoc-html

install: install-builddoc-html

uninstall: uninstall-builddoc-html

clean: clean-doc-html

install-builddoc-html: $(SOURCES)
	for i in $^; do                          \
	  DST=$(BUILDHTML)/$${i%%.xml}/;         \
	  $(MKDIR) -p $$DST;                     \
	  $(XSLTPROC) --xinclude                 \
	    --stringparam base.dir "$$DST"       \
	    $(HTMLXSL) $$i;                      \
	done

install-doc-html: BUILDHTML=$(DOCHTML)
install-doc-html: install-builddoc-html

uninstall-doc-html: BUILDHTML=$(DOCHTML)
uninstall-doc-html: clean-doc-html

clean-doc-html: $(SOURCES)
	for i in $^; do                         \
	  $(RM) -r $(BUILDHTML)/$${i%%.xml};    \
	done


