##########################################################################
#  Ocaml-gettext : example code                                           #
#                                                                         #
#  Copyright (C) 2003, 2004, 2005 Sylvain Le Gall <sylvain@le-gall.net>   #
#                                                                         #
#  You can redistribute this library and/or modify it under the terms of  #
#  the GNU LGPL v2.1 with the OCaml static compilation exception.         #
#                                                                         #
#  Contact: sylvain@le-gall.net                                           #
##########################################################################

all: library.cma

clean:
	$(RM) *.cma *.cmi *.cmo

library.cma: libraryGettext.ml library.ml 
	ocamlfind ocamlc -package gettext.base -a -o $@ $^
	
