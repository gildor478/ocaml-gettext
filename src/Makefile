NAME       = gettext
CMO        = gettext.cmo 
LIBRARY    = true
REQUIRES   =
PREDICATES =
INSTALLIB  =      \
 gettext.cmi      \
 gettext.cmxa     \
 gettext.cma      \
 pa_gettext.cma   \
 gettext.a        \
 dllgettext.so    \
 libgettext.a     \
 META
STUBSOBJS = -L/usr/lib/ocaml/3.07/ -lcamlidl

all: pa_gettext.cma
include ../TopMakefile

clean::
	rm -f gettext.ml gettext.mli gettext.h 
	rm -f dll*.so lib*.a
	rm -f gettext_stubs.c gettext_stubs.o
	rm -f pa_gettext.cma pa_gettext.ml

gettext.cmo: gettext.cmi
gettext.cmx: gettext.cmi
pa_gettext.cma: pa_gettext.ml
	$(OCAMLC) -I +camlp4 -a -o pa_gettext.cma pa_gettext.ml
pa_gettext.ml: pa_gettext.ml4
	$(OCAMLP4)  pa_o.cmo pa_extend.cmo q_MLast.cmo \
	pa_extend.cmo q_MLast.cmo pr_o.cmo -impl $< > $@
