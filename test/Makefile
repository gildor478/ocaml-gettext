NAME       = test
CMO        = test.cmo
REQUIRES   = fileutils gettext.extension gettext-camomile gettext-stub oUnit

include ../TopMakefile

TESTDIR=tests

run-test:
	./$(NAME) -test-dir $(TESTDIR)

clean::
	-$(RM) -r $(TESTDIR)
