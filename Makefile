all:
	cd libgettext-ocaml          && $(MAKE) all
	cd libgettext-stub-ocaml     && $(MAKE) all
	cd libgettext-camomile-ocaml && $(MAKE) all

install:
	cd libgettext-ocaml          && $(MAKE) install-lib
	cd libgettext-stub-ocaml     && $(MAKE) install-lib
	cd libgettext-camomile-ocaml && $(MAKE) install-lib

uninstall:
	cd libgettext-ocaml          && $(MAKE) uninstall-lib
	cd libgettext-stub-ocaml     && $(MAKE) uninstall-lib
	cd libgettext-camomile-ocaml && $(MAKE) uninstall-lib
	
clean:
	-cd libgettext-ocaml          && $(MAKE) clean
	-cd libgettext-stub-ocaml     && $(MAKE) clean
	-cd libgettext-camomile-ocaml && $(MAKE) clean
	-cd test                      && $(MAKE) clean
	-cd examples                  && $(MAKE) clean

distclean: clean
	-$(RM) config.* TopMakefile
	-$(RM) -r .libs
	-$(RM) src/.depend
	-$(RM) test/.depend
