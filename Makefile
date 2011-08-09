##########################################################################
#  ocaml-gettext: a library to translate messages                        #
#                                                                        #
#  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         #
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
##########################################################################

all:
	cd libgettext-ocaml          && $(MAKE) all
	cd libgettext-stub-ocaml     && $(MAKE) all
	cd libgettext-camomile-ocaml && $(MAKE) all
	cd ocaml-gettext             && $(MAKE) all
	cd po                        && $(MAKE) all
	cd doc                       && $(MAKE) all
	cd test                      && $(MAKE) all

install:
	cd libgettext-ocaml          && $(MAKE) install
	cd libgettext-stub-ocaml     && $(MAKE) install
	cd libgettext-camomile-ocaml && $(MAKE) install
	cd ocaml-gettext             && $(MAKE) install
	cd po                        && $(MAKE) install
	cd doc                       && $(MAKE) install

uninstall:
	cd doc                       && $(MAKE) uninstall
	cd po                        && $(MAKE) uninstall
	cd ocaml-gettext             && $(MAKE) uninstall
	cd libgettext-camomile-ocaml && $(MAKE) uninstall
	cd libgettext-stub-ocaml     && $(MAKE) uninstall
	cd libgettext-ocaml          && $(MAKE) uninstall
	
clean:
	-cd test                      && $(MAKE) clean
	-cd doc                       && $(MAKE) clean
	-cd po                        && $(MAKE) clean
	-cd examples                  && $(MAKE) clean
	-cd ocaml-gettext             && $(MAKE) clean
	-cd libgettext-camomile-ocaml && $(MAKE) clean
	-cd libgettext-stub-ocaml     && $(MAKE) clean
	-cd libgettext-ocaml          && $(MAKE) clean
	-$(RM) -r $(TMPBUILDDIR)

distclean: clean
	-cd test                      && $(MAKE) distclean
	-cd doc                       && $(MAKE) distclean
	-cd po                        && $(MAKE) distclean
	-cd examples                  && $(MAKE) distclean
	-cd ocaml-gettext             && $(MAKE) distclean
	-cd libgettext-camomile-ocaml && $(MAKE) distclean
	-cd libgettext-stub-ocaml     && $(MAKE) distclean
	-cd libgettext-ocaml          && $(MAKE) distclean
	-$(RM) config.* ConfMakefile
	-$(RM) -r autom4te.cache config.log config.cache config.status

headache: distclean
	headache -h .header -c .headache.config `find $(CURDIR)/ -type d -name .svn -prune -false -o -type f`

DIST_DIR=$(PACKAGE_TARNAME)-$(VERSION)
dist:
	darcs dist -d $(DIST_DIR)

test: all
	cd test && ./test

-include ConfMakefile

.PHONY: all install uninstall clean distclean dist test
