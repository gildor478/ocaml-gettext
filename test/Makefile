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
