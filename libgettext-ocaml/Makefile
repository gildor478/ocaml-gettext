NAME       = gettext
CMO        =       
LIBRARY    = true
REQUIRES   = fileutils
PREDICATES =
INSTALLIB  = META

all:

distclean::
	-$(RM) gettextConfig.ml

##############################
# Ocaml Gettext base library #
##############################

INSTALLIB +=         \
 gettextBase.cma     \
 gettextBase.cmxa    \
 gettextBase.a       \
 gettextTypes.cmi    \
 gettextMo.cmi       \
 gettextModules.cmi  \
 gettextCompat.cmi   \
 gettextConfig.cmi   \
 gettext.cmi         

CMO_BASE  =            \
 gettextTypes.cmo      \
 gettextUtils.cmo      \
 gettextMo_int32.cmo   \
 gettextMo_parser.cmo  \
 gettextMo_lexer.cmo   \
 gettextMo.cmo         \
 gettextModules.cmo    \
 gettextCompat.cmo     \
 gettextConfig.cmo     \
 gettext.cmo           

gettext.cmi: gettext.mli
gettext.cmo: gettext.cmi
gettextMo_parser.cmo: gettextMo_parser.cmi
gettextMo_parser.cmx: gettextMo_parser.cmi
gettextCompat.cmo: gettextCompat.cmi
gettextBase.cma: $(CMO_BASE)
gettextBase.cmxa: $(CMO_BASE:.cmo=.cmx)

clean::
	-$(RM) gettextMo_parser.output \
	 gettextMo_parser.ml gettextMo_parser.mli

#################################
# Ocaml Gettext compile library #
#################################

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
gettextCompile.cma: $(CMO_COMPILE)
gettextCompile.cmxa: $(CMO_COMPILE:.cmo=.cmx)

clean::
	-$(RM) gettextPo_parser.output \
	 gettextPo_parser.ml gettextPo_parser.mli

###############################
# Ocaml Gettext extension kit #
###############################

INSTALLIB +=           \
 gettextExtension.cma  \
 gettextExtension.cmxa \
 gettextExtension.a    \
 gettextLocale.cmi     \
 gettextDomain.cmi     \
 gettextCharset.cmi    \
 gettextTranslate.cmi  

CMO_EXTENSION=         \
 gettextConfig.cmo     \
 gettextLocale.cmo     \
 gettextDomain.cmo     \
 gettextCharset.cmo    \
 gettextTranslate.cmo  

gettextExtension.cma: $(CMO_EXTENSION)
gettextExtension.cmxa: $(CMO_EXTENSION:.cmo=.cmx)

###############################
# Pa_gettext extension        #
###############################

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
