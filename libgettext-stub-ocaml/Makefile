NAME       = gettext-stub
CMO        =       
LIBRARY    = true
REQUIRES   = gettext.extension
PREDICATES =
INSTALLIB  = META

all:

distclean::
	-$(RM) META

##############################
# Stub library               #
##############################

INSTALLIB +=               \
 gettextStub.cmxa          \
 gettextStub.cma           \
 gettextStub.a             \
 dllgettextStub.so         \
 libgettextStub.a          \
 gettextStubCompat.cmi     

CMO_STUB =                 \
 gettextStubCompat_stubs.o \
 gettextStubCompat.cmo     \
 gettextStub.cmo 

gettextStubCompat.cmo: gettextStubCompat.cmi
gettextStubCompat.cmx: gettextStubCompat.cmi
gettextStub.cma: $(CMO_STUB)
gettextStub.cmxa: $(CMO_STUB:.cmo=.cmx)
gettextStub.cma gettextStub.cmxa: STUBSOBJS = -L$(OCAMLLIB) -lcamlidl

clean::
	-$(RM) gettextStubCompat.ml gettextStubCompat.mli 
	-$(RM) gettextStubCompat.h gettextStubCompat_stubs.c 

include ../TopMakefile
