NAME = camlgettext
CMO = \
	camlgettext_parser.cmo \
	camlgettext_lexer.cmo  \
	camlgettext.cmo 
LIBRARY = true
REQUIRES =
PREDICATES =
INSTALLIB = \
	camlgettext.cmi\
	camlgettext.cmxa\
	camlgettext.cma\
	pa_gettext.cma\
	camlgettext.a\
 	META
STUBSOBJS = -L/usr/lib/ocaml/3.07/ -lcamlidl

all: pa_gettext.cma
include ../TopMakefile

clean::
	$(RM) camlgettext_parser.ml
	$(RM) camlgettext_parser.mli
	$(RM) camlgettext_parser.output
	$(RM) camlgettext_lexer.ml
	$(RM) pa_gettext.cma 
	$(RM) pa_gettext.ml

camlgettext_parser.cmo: camlgettext_parser.cmi
camlgettext_parser.cmx: camlgettext_parser.cmi

pa_gettext.cma: pa_gettext.ml
	$(OCAMLC) -I +camlp4 -a -o pa_gettext.cma pa_gettext.ml
pa_gettext.ml: pa_gettext.ml4
	$(OCAMLP4)  pa_o.cmo pa_extend.cmo q_MLast.cmo \
	pa_extend.cmo q_MLast.cmo pr_o.cmo -impl $< > $@
