REQUIRES   = fileutils gettext.extension gettext-camomile gettext-stub oUnit benchmark
TESTDIR    = tests
CMO        = common.cmo benchmark.cmo test.cmo

all: all-benchmark all-test

include ../TopMakefile

all-benchmark install-benchmark clean-benchmark: NAME = benchmark
all-benchmark install-benchmark clean-benchmark: CMO  = common.cmo benchmark.cmo
all-benchmark: all-$(OCAMLBEST)
install-benchmark: install-$(OCAMLBEST)
clean-benchmark: clean

all-test install-test clean-test: NAME = test
all-test install-test clean-test: CMO  = common.cmo test.cmo
all-test: all-$(OCAMLBEST)
install-test: install-$(OCAMLBEST)
clean-test: clean

clean::
	-$(RM) -r $(TESTDIR)
	
