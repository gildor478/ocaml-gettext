NAME       = test
CMO        = test.cmo
REQUIRES   = fileutils gettext.base gettext.compile oUnit

include ../TopMakefile
all: test4.opo

clean::
	$(RM) *.mo

test4.opo: test4.ml
	CMO_EXTRACT=`$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -o-format`
	CMA_EXTRACT=`$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -a-format`
	INC_EXTRACT=`$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -i-format`
	camlp4 -I                                                                           \
	   +camlp4                                                                          \
	  `$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -i-format` \
	  pa_o.cmo                                                                          \
	  `$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -a-format` \
	  `$(OCAMLFIND) query -r -predicates preprocessor,syntax gettext.extract -o-format` \
	  -o $@ $< 
