NAME       = test
CMO        = test.cmo
REQUIRES   = fileutils gettext.base gettext.extension oUnit

include ../TopMakefile

TEST_FILES=   \
  test1.mo    \
  test2.mo    \
  test3.mo    \
  fr.test3.mo \
  us.test3.mo \
  test4.pot   

TEST_DIRS =   \
  fr

clean::
	-$(RM) $(TEST_FILES)
	-$(RM) -r $(TEST_DIRS)
