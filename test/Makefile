REQUIRES   = fileutils gettext.extension gettext-camomile gettext-stub oUnit benchmark
TESTDIR    = tests
CMO        = common.cmo benchmark.cmo test.cmo

all: all-benchmark
	$(MAKE) all-test

install: install-benchmark
	$(MAKE) install-test

uninstall: uninstall-benchmark
	$(MAKE) uninstall-test

clean:: clean-benchmark
	$(MAKE) clean-test
	-$(RM) -r $(TESTDIR)

distclean::
	
include ../TopMakefile

all-benchmark install-benchmark uninstall-benchmark clean-benchmark: NAME = benchmark
all-benchmark install-benchmark uninstall-benchmark clean-benchmark: CMO  = common.cmo benchmark.cmo
all-benchmark: all-$(OCAMLBEST)
install-benchmark: install-$(OCAMLBEST)
uninstall-benchmark: uninstall-prog
clean-benchmark: clean-prog

all-test install-test uninstall-test clean-test: NAME = test
all-test install-test uninstall-test clean-test: CMO  = common.cmo test.cmo
all-test: all-$(OCAMLBEST)
install-test: install-$(OCAMLBEST)
uninstall-test: uninstall-prog
clean-test: clean-prog
