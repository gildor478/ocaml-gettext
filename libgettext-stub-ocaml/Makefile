NAME       = gettext-stub
CMO        =       
LIBRARY    = true
REQUIRES   = fileutils 
PREDICATES =
INSTALLIB  = META

all:

distclean::
	-$(RM) META

##############################
# Stub library               #
##############################

INSTALLIB +=               \
 gettextStubCompat.cmi     \
 gettextStubCompat.cmxa    \
 gettextStubCompat.cma     \
 gettextStubCompat.a       \
 dllgettextStubCompat.so   \
 libgettextStubCompat.a   

CMO_STUB =                 \
 gettextStubCompat_stubs.o \
 gettextStubCompat.cmo 

gettextStubCompat.cmo: gettextStubCompat.cmi
gettextStubCompat.cmx: gettextStubCompat.cmi
gettextStubCompat.cma: $(CMO_STUB)
gettextStubCompat.cmxa: $(CMO_STUB:.cmo=.cmx)
gettextStubCompat.cma gettextStubCompat.cmxa: STUBSOBJS = -L$(OCAMLLIB) -lcamlidl

clean::
	-$(RM) gettextStubCompat.ml gettextStubCompat.mli 
	-$(RM) gettextStubCompat.h gettextStubCompat_stubs.c 

include ../TopMakefile
