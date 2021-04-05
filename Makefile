LIBDIR = lib
LIB = $(LIBDIR)/OCamlFlat
VERSION = 1.0
COMP = ocamlc
FLAGS =
NAMES =		OCamlFlat Error Util Set Scanner JSon RegExpSyntax			\
			CFGSyntax BasicTypes Configuration Examples					\
			Entity Exercise Model FiniteAutomaton RegularExpression		\
			ContextFreeGrammar FiniteEnumeration PolyModel TopLevel		\
			LearnOCaml Tests PreOpen

define SRCFILES
	$(addprefix src/, $(addsuffix .ml, $(foreach file, $(NAMES), $(file))))
endef

$(LIB).cma: $(LIB).ml
	$(COMP) $(FLAGS) -o $(LIB).cma -a $(LIB).ml
	@rm $(LIB).cmi $(LIB).cmo

$(LIB).ml: $(LIBDIR) $(SRCFILES)
	cat $(SRCFILES) > $(LIB).ml

$(LIBDIR):
	mkdir lib

.PHONY: run
run: $(LIB).ml
	rlwrap ocaml -init $(LIB).ml

# run some unit tests
.PHONY: test
test: $(LIB).ml
	cp $(LIB).ml test.ml
	echo ";; tests ();;  #quit;;" >> test.ml
	ocaml -init test.ml
	@rm test.ml

.PHONY: types
types: $(LIB).ml
	echo "#use \"lib/OCamlFlat.ml\";;" | ocaml;

clean:
	rm -rf $(LIBDIR)
