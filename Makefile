LIBDIR = lib
LIB = $(LIBDIR)/OCamlFlat
VERSION = 1.0
COMP = ocamlc
FLAGS =
NAMES =		OCamlFlat Configuration Error Util Set Scanner JSon		\
			BasicTypes RegExpSyntax CFGSyntax Examples	\
			Entity Exercise Model FiniteAutomaton RegularExpression	\
			ContextFreeGrammar RDParser RDParserWithDeclarations RDParserWithoutDeclarations \
			RDParserNeedFunDeclaration RDParserNeedRecursiveFun \
			RDParserC RDParserOCaml RDParserJava LL1Grammar \
			FiniteEnumeration PolyModel TopLevel	\
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

.PHONY: edit
edit:
	geany OCamlFlat.geany src/*

.PHONY: dist
dist: $(LIBDIR)
	rm -rf OCamlFlat
	mkdir OCamlFlat
	cp -a LICENCE Makefile README.md OCamlFlat.geany src OCamlFlat
	tar cpvz OCamlFlat > $(LIBDIR)/OCamlFlat.tgz
	rm -rf OCamlFlat

.PHONY: git0
git0:
	git status

.PHONY: git1
git1:
	git diff

.PHONY: git2
git2:
	git add src/* Makefile OCamlFlat.geany
	git status

.PHONY: git3
git3:
	git commit
	git status

.PHONY: git4
git4:
	git push origin master

.PHONY: test
test: $(LIB).ml
	TESTING=true ocaml $(LIB).ml

.PHONY: types
types: $(LIB).ml
	echo "#use \"lib/OCamlFlat.ml\";;" | ocaml;

clean:
	rm -rf $(LIBDIR)
