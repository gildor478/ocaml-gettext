##########################################################################
#  Ocaml-gettext : a library to translate messages                       #
#                                                                        #
#  Copyright (C) 2003, 2004, 2005 Sylvain Le Gall <sylvain@le-gall.net>  #
#                                                                        #
#  This library is free software; you can redistribute it and/or         #
#  modify it under the terms of the GNU Lesser General Public            #
#  License as published by the Free Software Foundation; either          #
#  version 2.1 of the License, or (at your option) any later version;    #
#  with the OCaml static compilation exception.                          #
#                                                                        #
#  This library is distributed in the hope that it will be useful,       #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  Lesser General Public License for more details.                       #
#                                                                        #
#  You should have received a copy of the GNU Lesser General Public      #
#  License along with this library; if not, write to the Free Software   #
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   #
#  USA                                                                   #
#                                                                        #
#  Contact: sylvain@le-gall.net                                          #
##########################################################################

REQUIRES   = fileutils gettext.extension gettext-camomile gettext-stub oUnit benchmark
TESTDIR    = tests
CMO        = common.cmo bench.cmo test.cmo

all: all-bench
	$(MAKE) all-test

install: install-bench
	$(MAKE) install-test

uninstall: uninstall-bench
	$(MAKE) uninstall-test

clean:: clean-bench
	$(MAKE) clean-test
	-$(RM) -r $(TESTDIR)

distclean::
	
include ../ConfMakefile
include ../TopMakefile

all-bench install-bench uninstall-bench clean-bench: NAME = bench
all-bench install-bench uninstall-bench clean-bench: CMO  = common.cmo bench.cmo
all-bench: all-$(OCAMLBEST)
install-bench: install-$(OCAMLBEST)
uninstall-bench: uninstall-prog
clean-bench: clean-prog

all-test install-test uninstall-test clean-test: NAME = test
all-test install-test uninstall-test clean-test: CMO  = common.cmo test.cmo
all-test: all-$(OCAMLBEST)
install-test: install-$(OCAMLBEST)
uninstall-test: uninstall-prog
clean-test: clean-prog
