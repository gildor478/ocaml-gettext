DOCBOOKTOPDF=docbook2pdf
DOCBOOKTOHTML=docbook2html
BUILDHTML=html
MKDIR=mkdir

SOURCES=reference-manual.xml

all: install-builddoc-html

install: install-builddoc-html

uninstall: uninstall-builddoc-html

clean: clean-doc-html

install-builddoc-html: $(SOURCES)
	for i in $^; do                          \
	  DST=$(BUILDHTML)/$${i%%.xml};          \
	  $(MKDIR) -p $$DST;                     \
	  $(DOCBOOKTOHTML) -o $$DST $$i;         \
	done

install-doc-html: $(SOURCES) 
	for i in $^; do                          \
	  DST=$(DOCHTML)/$${i%%.xml};            \
	  $(MKDIR) -p $$DST;                     \
	  $(DOCBOOKTOHTML) -o $$DST $$i;         \
	done

uninstall-doc-html: $(SOURCES)
	for i in $^; do                         \
	  $(RM) -r $(DOCHTML)/$${i%%.xml};      \
	done

clean-doc-html: $(SOURCES)
	for i in $^; do                         \
	  $(RM) -r $(BUILDHTML)/$${i%%.xml};    \
	done


