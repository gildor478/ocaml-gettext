NAME = camlgettext
CMO = \
        camlgettext_types.cmo  \
	camlgettext_int32.cmo  \
	camlgettext_parser.cmo \
	camlgettext_lexer.cmo  \
	camlgettext.cmo 
REQUIRES =
PREDICATES =
INSTALLIB = \
	camlgettext.cmi\
	camlgettext.cmxa\
	camlgettext.cma\
 	META

include ../TopMakefile

camlgettext_parser.cmo: camlgettext_parser.cmi
camlgettext_parser.cmx: camlgettext_parser.cmi

clean::
	-$(RM) camlgettext_parser.mli camlgettext_parser.output camlgettext_parser.ml
