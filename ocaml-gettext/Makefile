###################
# Standard target #
###################

all:

install:
	
uninstall:
	
clean::
	
distclean::
	
#################################
# Ocaml Gettext utility program #
#################################

NAME       = ocaml-gettext
CMO        = ocaml-gettext.cmo      
REQUIRES   = gettext fileutils
PREDICATES =


#################################
# Ocaml Gettext extract program #
#################################

all: ocaml-xgettext
ocaml-xgettext:
	$(MKCAMLP4)                                                                         \
           -I +camlp4 pa_o.cmo                                                              \
          `$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -i-format` \
          `$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -a-format` \
          `$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -o-format` \
          -o $@
	$(CP) $@ $(BUILDBIN)


install: ocaml-xgettext-install
ocaml-xgettext-install:
	$(CP) ocaml-xgettext $(BINDIR)

uninstall: ocaml-xgettext-uninstall
ocaml-xgettext-uninstall:
	-$(RM) $(BINDIR)/ocaml-xgettext 

clean:: 
	-$(RM) ocaml-xgettext

distclean::

include ../TopMakefile
