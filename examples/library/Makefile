
all: library.cma

clean:
	$(RM) *.cma *.cmi *.cmo

library.cma: libraryGettext.ml library.ml 
	ocamlfind ocamlc -package gettext.base -a -o $@ $^
	
