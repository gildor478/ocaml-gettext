NAME = camlgettext
CMO = camlgettext.cmo 
LIBRARY = true
REQUIRES =
PREDICATES =
INSTALLIB = camlgettext.cmi\
 camlgettext.cmxa\
 camlgettext.cma\
 pa_gettext.cma\
 camlgettext.a\
 dllcamlgettext.so\
 libcamlgettext.a\
 META
STUBSOBJS = -L/usr/lib/ocaml/3.06/ -lcamlidl

all: pa_gettext.cma
include ../TopMakefile

clean::
	rm -f camlgettext.ml camlgettext.mli camlgettext.h 
	rm -f dll*.so lib*.a
	rm -f camlgettext_stubs.c camlgettext_stubs.o
	rm -f pa_gettext.cma pa_gettext.ml

camlgettext.cmo: camlgettext.cmi
camlgettext.cmx: camlgettext.cmi
pa_gettext.cma: pa_gettext.ml
	$(OCAMLC) -I +camlp4 -a -o pa_gettext.cma pa_gettext.ml
pa_gettext.ml: pa_gettext.ml4
	$(OCAMLP4)  pa_o.cmo pa_extend.cmo q_MLast.cmo \
	pa_extend.cmo q_MLast.cmo pr_o.cmo -impl $< > $@
