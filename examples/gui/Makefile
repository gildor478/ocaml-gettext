all: gui.cma

clean: 
	$(RM) *.cma *.cmi *.cmo

gui.cma: guiGettext.ml gui.ml
	ocamlfind ocamlc -package "gettext.base lablgtk2" -a -o $@ $^
