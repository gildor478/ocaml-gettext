NAME       = test
CMO        = test.cmo
REQUIRES   = fileutils gettext.base gettext.extension oUnit

include ../TopMakefile

TEST_FILES=         \
  test1.mo          \
  test2.mo          \
  test3.mo          \
  fr.test3.mo       \
  us.test3.mo       \
  test4.pot         \
  test4.po.bak      \
  test4.mo          \
  mydomain.test4.mo

TEST_DIRS =    \
  fr           \
  fr_FR        \
  fr_FR@euro

clean::
	-$(RM) $(TEST_FILES)
	-$(RM) -r $(TEST_DIRS)
