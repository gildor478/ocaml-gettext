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

all: program.1

clean:
	$(RM) program.1

program.1: program.xml
	xmllint --xinclude --noent --noout --postvalid $^
	xsltproc --xinclude /usr/share/xml/docbook/stylesheet/nwalsh/manpages/docbook.xsl $^
