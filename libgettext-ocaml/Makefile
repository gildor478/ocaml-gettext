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

# Ocaml Gettext base library #

INSTALLIB +=         \
 gettextBase.cma     \
 gettextBase.cmxa    \
 gettextBase.a       \
 gettextTypes.cmi    \
 gettextMo.cmi    

CMO_BASE  =            \
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
	-$(RM) gettextMo_parser.output \
	 gettextMo_parser.ml gettextMo_parser.mli

# Ocaml Gettext compile library */

INSTALLIB +=           \
 gettextCompile.cma    \
 gettextCompile.cmxa   \
 gettextCompile.a      \
 gettextPo.cmi         

CMO_COMPILE =          \
 gettextPo_parser.cmo  \
 gettextPo_lexer.cmo   \
 gettextPo.cmo

gettextPo_parser.cmo: gettextPo_parser.cmi
gettextPo_parser.cmx: gettextPo_parser.cmi
gettextCompile.cma gettextCompile.cmxa: $(CMO_COMPILE)
gettextCompile.cma gettextCompile.cmxa: $(CMO_COMPILE:.cmo=.cmx)

clean::
	-$(RM) gettextPo_parser.output \
	 gettextPo_parser.ml gettextPo_parser.mli

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
