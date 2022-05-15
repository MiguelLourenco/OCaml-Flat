LIBDIR = lib
LIB = $(LIBDIR)/OCamlFlat
VERSION = 1.0
COMP = ocamlc
FLAGS =
NAMES =		OCamlFlat Configuration Error Set BasicTypes Util Scanner JSon	\
			RegExpSyntax CFGSyntax Examples									\
			Entity Exercise Model FiniteAutomaton RegularExpression			\
			ContextFreeGrammar RDParser LL1Grammar LRGrammar				\
			FiniteEnumeration PolyModel TopLevel LearnOCaml Tests PreOpen

define SRCFILES
	$(addprefix src/, $(addsuffix .ml, $(foreach file, $(NAMES), $(file))))
endef

define SRCFILES2
	$(addsuffix .ml, $(foreach file, $(NAMES), $(file)))
endef

$(LIB).cma: $(LIB).ml
	$(COMP) $(FLAGS) -o $(LIB).cma -a $(LIB).ml
	@rm $(LIB).cmi $(LIB).cmo

$(LIB).ml: $(LIBDIR) $(SRCFILES)
	cat $(SRCFILES) > $(LIB).ml

.PHONY: type_check
type_check: $(LIBDIR)
	@cd src ; ocamlc -c OCamlFlat.ml
	@cd src ; ocamlc -c Configuration.ml
	@cd src ; ocamlc -c -open Configuration Error.ml
	@cd src ; ocamlc -c -open Configuration -open Error Set.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set BasicTypes.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes Util.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util Scanner.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner JSon.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon RegExpSyntax.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax CFGSyntax.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax Examples.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples Entity.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity Exercise.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise Model.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model FiniteAutomaton.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton RegularExpression.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression ContextFreeGrammar.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar RDParser.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser LL1Grammar.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser -open LL1Grammar LRGrammar.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser -open LL1Grammar -open LRGrammar FiniteEnumeration.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser -open LL1Grammar -open LRGrammar -open FiniteEnumeration PolyModel.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser -open LL1Grammar -open LRGrammar -open FiniteEnumeration -open PolyModel TopLevel.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser -open LL1Grammar -open LRGrammar -open FiniteEnumeration -open PolyModel -open TopLevel LearnOCaml.ml
	@cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser -open LL1Grammar -open LRGrammar -open FiniteEnumeration -open PolyModel -open TopLevel -open LearnOCaml Tests.ml
	 cd src ; ocamlc -c -open Configuration -open Error -open Set -open BasicTypes -open Util -open Scanner -open JSon -open RegExpSyntax -open CFGSyntax -open Examples -open Entity -open Exercise -open Model -open FiniteAutomaton -open RegularExpression -open ContextFreeGrammar -open RDParser -open LL1Grammar -open LRGrammar -open FiniteEnumeration -open PolyModel -open TopLevel -open LearnOCaml -open Tests PreOpen.ml
	@cd src ; rm *.cmi *.cmo

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
	tar cpvz OCamlFlat > OCamlFlat.tgz
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
test: type_check $(LIB).ml
	TESTING=true ocaml $(LIB).ml

.PHONY: types
types: type_check $(LIB).ml
	echo "#use \"lib/OCamlFlat.ml\";;" | ocaml;

clean:
	rm -rf $(LIBDIR)
