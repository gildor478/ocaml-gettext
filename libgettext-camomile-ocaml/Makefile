NAME       = gettext-camomile
CMO        =       
LIBRARY    = true
REQUIRES   = fileutils gettext.extension camomile
PREDICATES =
INSTALLIB  = META

all:

distclean::
	-$(RM) META

##########################
# Ocaml Gettext Camomile #
##########################

REQUIRES += camomile

INSTALLIB +=          \
 gettextCamomile.cma  \
 gettextCamomile.cmxa \
 gettextCamomile.a    \
 gettextCamomile.cmi

CMO_CAMOMILE=         \
 gettextCamomile.cmo

gettextCamomile.cma: $(CMO_CAMOMILE)
gettextCamomile.cmxa: $(CMO_CAMOMILE:.cmo=.cmx)

include ../TopMakefile
