NAME = camlgettext
CMO = camlgettext.cmo 
LIBRARY = true
REQUIRES =
PREDICATES =
INSTALLIB = camlgettext.cmi camlgettext.cmxa camlgettext.cma camlgettext.a dllcamlgettext.so libcamlgettext.a META
STUBSOBJS = -L/usr/lib/ocaml/3.06/ -lcamlidl

include ../TopMakefile

clean::
	rm -f camlgettext.ml camlgettext.mli camlgettext.h 
	rm -f dll*.so lib*.a
	rm -f camlgettext_stubs.c camlgettext_stubs.o

camlgettext.cmo: camlgettext.cmi
camlgettext.cmx: camlgettext.cmi
