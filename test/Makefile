NAME = test
CMO = test.cmo
REQUIRES = camlgettext unix
PREDICATES =
SYNTAX = camlp4o
include ../TopMakefile

clean::
	$(RM) $(NAME).pot

ifdef SYNTAX
PA_CMO = `$(OCAMLFIND) query $(REQUIRES) -predicates "syntax preprocessor $(SYNTAX)" -a-format -recursive -separator ' '`
PA_INC = `$(OCAMLFIND) query $(REQUIRES) -predicates "syntax preprocessor $(SYNTAX)" -i-format -recursive -separator ' '`

$(NAME).pot: $(CMO:.cmo=.ml)
	$(RM) $(NAME).pot
	touch $(NAME).pot
	@for i in *.ml; do \
	 $(OCAMLP4) $(PA_INC) $(PA_CMO) pr_o.cmo $$i | xgettext -j -L Ocaml -o $(NAME).pot -;\
	done
else
$(NAME).pot: $(CMO:.cmo=.ml)
	$(RM) $(NAME).pot
	xgettext -j -o $(NAME).pot $<
endif
