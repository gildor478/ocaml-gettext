NAME       = gettext
CMO        =       
LIBRARY    = true
REQUIRES   =
PREDICATES =
INSTALLIB  = META

all:

# Stub library #
INSTALLIB +=         \
 gettextStub.cmi     \
 gettextStub.cmxa    \
 gettextStub.cma     \
 gettextStub.a       \
 dllgettextStub.so   \
 libgettextStub.a   

CMO_STUB =           \
 gettextStub_stubs.o \
 gettextStub.cmo 

gettextStub.cmo: gettextStub.cmi
gettextStub.cmx: gettextStub.cmi
gettextStub.cma gettextStub.cmxa: $(CMO_STUB)
gettextStub.cma gettextStub.cmxa: $(CMO_STUB:.cmo=.cmx)
gettextStub.cma gettextStub.cmxa: STUBSOBJS = -L/usr/lib/ocaml/3.07/ -lcamlidl

clean::
	-$(RM) gettextStub.ml gettextStub.mli gettextStub.h gettextStub_stubs.c 

# Ocaml base library #

INSTALLIB +=         \
 gettextBase.cma     \
 gettextBase.cmxa    \
 gettextTypes.cmi    \
 gettextMo.cmi    

CMO_BASE  +=           \
 gettextTypes.cmo      \
 gettextMo_int32.cmo   \
 gettextMo_parser.cmo  \
 gettextMo_lexer.cmo   \
 gettextMo.cmo     

gettextMo_parser.cmo: gettextMo_parser.cmi
gettextMo_parser.cmx: gettextMo_parser.cmi
gettextBase.cma gettextBase.cmxa: $(CMO_BASE)
gettextBase.cma gettextBase.cmxa: $(CMO_BASE:.cmo=.cmx)

clean::
	-$(RM) gettextParser.mli gettext_parser.output gettext_parser.ml

# Pa_gettext extension #
INSTALLIB +=        \
 pa_gettext.cma    

pa_gettext.cma: pa_gettext.ml
	$(OCAMLC) -I +camlp4 -a -o pa_gettext.cma pa_gettext.ml
pa_gettext.ml: pa_gettext.ml4
	$(OCAMLP4)  pa_o.cmo pa_extend.cmo q_MLast.cmo \
	pa_extend.cmo q_MLast.cmo pr_o.cmo -impl $< > $@

clean::
	-$(RM) pa_gettext.ml


include ../TopMakefile
