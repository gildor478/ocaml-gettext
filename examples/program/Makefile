all: program

clean:
	$(RM) *.cmi *.cmo program

program: programGettext.ml program.ml
	ocamlfind ocamlc -package "gettext-camomile lablgtk2.init" -linkpkg \
	  -I ../library -I ../gui -o $@ library.cma gui.cma $^
	
