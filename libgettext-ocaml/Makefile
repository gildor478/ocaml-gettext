NAME       = gettext
CMO        =       
LIBRARY    = true
REQUIRES   = fileutils
PREDICATES =
INSTALLIB  = META

all:

distclean::
	-$(RM) gettextConfig.ml
	-$(RM) META

##############################
# Ocaml Gettext base library #
##############################

INSTALLIB +=              \
 gettextBase.cma          \
 gettextBase.cmxa         \
 gettextBase.a            \
 gettextConfig.cmi        \
 gettextTypes.cmi         \
 gettextModules.cmi       \
 gettextCompat.cmi        \
 gettext.cmi         

CMO_BASE  =               \
 gettextConfig.cmo        \
 gettextTypes.cmo         \
 gettextUtils.cmo         \
 gettextFormat_parser.cmo \
 gettextFormat_lexer.cmo  \
 gettextFormat.cmo        \
 gettextMo_int32.cmo      \
 gettextMo_parser.cmo     \
 gettextMo_lexer.cmo      \
 gettextMo.cmo            \
 gettextModules.cmo       \
 gettextCompat.cmo        \
 gettext.cmo           

gettext.cmi: gettext.mli
gettext.cmo: gettext.cmi
gettextMo_parser.cmo: gettextMo_parser.cmi
gettextMo_parser.cmx: gettextMo_parser.cmi
gettextFormat_parser.cmo: gettextFormat_parser.cmi
gettextFormat_parser.cmx: gettextFormat_parser.cmi
gettextCompat.cmo: gettextCompat.cmi
gettextBase.cma: $(CMO_BASE)
gettextBase.cmxa: $(CMO_BASE:.cmo=.cmx)

clean::
	-$(RM) gettextMo_parser.output \
	 gettextMo_parser.ml gettextMo_parser.mli

###################################
# Ocaml Gettext extension library #
###################################

INSTALLIB +=           \
 gettextExtension.cma  \
 gettextExtension.cmxa \
 gettextExtension.a    \
 gettextFormat.cmi     \
 gettextMo.cmi         \
 gettextCategory.cmi   \
 gettextLocale.cmi     \
 gettextDomain.cmi     \
 gettextCharset.cmi    \
 gettextTranslate.cmi  \
 gettextRealize.cmi    \
 gettextPo.cmi         \
 gettextCompile.cmi         

CMO_EXTENSION=         \
 gettextConfig.cmo     \
 gettextCategory.cmo   \
 gettextLocale.cmo     \
 gettextDomain.cmo     \
 gettextCharset.cmo    \
 gettextTranslate.cmo  \
 gettextRealize.cmo    \
 gettextPo_utils.cmo   \
 gettextPo_parser.cmo  \
 gettextPo_lexer.cmo   \
 gettextPo.cmo         \
 gettextCompile.cmo

gettextPo_parser.cmo: gettextPo_parser.cmi
gettextPo_parser.cmx: gettextPo_parser.cmi
gettextExtension.cma: $(CMO_EXTENSION)
gettextExtension.cmxa: $(CMO_EXTENSION:.cmo=.cmx)

clean::
	-$(RM) gettextPo_parser.output \
	 gettextPo_parser.ml gettextPo_parser.mli

########################
# Pa_gettext extension #
########################

INSTALLIB +=        \
 pr_gettext.cmo    

pr_gettext.cmo: REQUIRES=ast-analyze

include ../TopMakefile
