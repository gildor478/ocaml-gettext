NAME       = test
CMO        = test.cmo
REQUIRES   = fileutils gettext.base gettext.extension oUnit

include ../TopMakefile

TESTDIR=tests

run-test:
	./$(NAME) -test-dir $(TESTDIR)

clean::
	-$(RM) -r $(TESTDIR)
