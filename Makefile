all:
	cd src && $(MAKE) all
	cd src-native && $(MAKE) all

install:
	cd src && $(MAKE) install-lib
	cd src-native && $(MAKE) install-lib

uninstall:
	cd src && $(MAKE) uninstall-lib
	cd src-native && $(MAKE) uninstall-lib
	
clean:
	cd src && $(MAKE) clean
	cd src-native && $(MAKE) clean
	cd test && $(MAKE) clean

distclean:
	-@if [ -e TopMakefile ]; then \
	 cd src && $(MAKE) clean; \
	 cd src-native && $(MAKE) clean; \
	 cd test && $(MAKE) clean; \
	fi
	-$(RM) config.* TopMakefile
	-$(RM) -r .libs
	-$(RM) src/.depend
	-$(RM) src-native/.depend
	-$(RM) test/.depend
