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
gettextStub.cma: $(CMO_STUB)
gettextStub.cmxa: $(CMO_STUB:.cmo=.cmx)
gettextStub.cma gettextStub.cmxa: STUBSOBJS = -L$(OCAMLLIB) -lcamlidl

clean::
	-$(RM) gettextStub.ml gettextStub.mli gettextStub.h gettextStub_stubs.c 

include ../TopMakefile
