all:
	cd src && $(MAKE) all

install:
	cd src && $(MAKE) install-lib

uninstall:
	cd src && $(MAKE) uninstall-lib
	
clean:
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean

distclean:
	@if [ -e TopMakefile ]; then \
	 cd src && $(MAKE) clean; \
	 cd test && $(MAKE) clean; \
	fi
	$(RM) config.* TopMakefile
	$(RM) -r .libs
	$(RM) src/.depends
	$(RM) test/.depends
