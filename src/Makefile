NAME       = gettext
CMO        =       
LIBRARY    = true
REQUIRES   =
PREDICATES =
INSTALLIB  = META

include ../TopMakefile

# Stub library #
CMO       +=        \
 gettextStub.cmo    \
INSTALLIB +=        \
 gettextStub.cmi    \
 gettextStub.cmxa   \
 gettextStub.cma    \
 gettextStub.a      \
 dllgettextStub.so  \
 libgettextStub.a   
STUBSOBJS = -L/usr/lib/ocaml/3.07/ -lcamlidl

gettextStub.cmo: gettextStub.cmi
gettextStub.cmx: gettextStub.cmi

clean::
	-$(RM) gettextStub.ml gettextStub.mli gettextStub.h 
	-$(RM) dllgettextStub.so libgettextStub.a
	-$(RM) gettextStub_stubs.c gettextStub_stubs.o

# Ocaml base library #
CMO       +=        \
 gettext_types.cmo  \
 gettext_int32.cmo  \
 gettext_parser.cmo \
 gettext_lexer.cmo  \
 gettext_mo.cmo     

gettext_parser.cmo: gettext_parser.cmi
gettext_parser.cmx: gettext_parser.cmi

clean::
	-$(RM) camlgettext_parser.mli camlgettext_parser.output camlgettext_parser.ml
	
# Pa_gettext extension #
INSTALLIB +=        \
 pa_gettext.cma    
 
all: pa_gettext.cma

pa_gettext.cma: pa_gettext.ml
	$(OCAMLC) -I +camlp4 -a -o pa_gettext.cma pa_gettext.ml
pa_gettext.ml: pa_gettext.ml4
	$(OCAMLP4)  pa_o.cmo pa_extend.cmo q_MLast.cmo \
	pa_extend.cmo q_MLast.cmo pr_o.cmo -impl $< > $@
clean::
	-$(RM) pa_gettext.cma pa_gettext.ml

