NAME       = gettextStub
CMO        = gettextStub.cmo 
LIBRARY    = true
REQUIRES   =
PREDICATES =
INSTALLIB  =       \
 gettextStub.cmi   \
 gettextStub.cmxa  \
 gettextStub.cma   \
 pa_gettext.cma    \
 gettextStub.a     \
 dllgettextStub.so \
 libgettextStub.a  \
 META
STUBSOBJS = -L/usr/lib/ocaml/3.07/ -lcamlidl

all: pa_gettext.cma
include ../TopMakefile

clean::
	rm -f gettextStub.ml gettextStub.mli gettextStub.h 
	rm -f dll*.so lib*.a
	rm -f gettextStub_stubs.c gettextStub_stubs.o
	rm -f pa_gettext.cma pa_gettext.ml

gettextStub.cmo: gettextStub.cmi
gettextStub.cmx: gettextStub.cmi
pa_gettext.cma: pa_gettext.ml
	$(OCAMLC) -I +camlp4 -a -o pa_gettext.cma pa_gettext.ml
pa_gettext.ml: pa_gettext.ml4
	$(OCAMLP4)  pa_o.cmo pa_extend.cmo q_MLast.cmo \
	pa_extend.cmo q_MLast.cmo pr_o.cmo -impl $< > $@
