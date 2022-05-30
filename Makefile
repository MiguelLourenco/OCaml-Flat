VERSION = 1.0
COMP = ocamlc
FLAGS =
LIB_DIR = lib
LIB_NAME = OCamlFlat
LIB = $(LIB_DIR)/$(LIB_NAME)
SRC_DIR = src
SERVICE = $(SRC_DIR)/Service.ml
NAMES =		OCamlFlat Configuration Error Set BasicTypes Util Scanner JSon	\
			RegExpSyntax CFGSyntax Examples									\
			Entity Exercise Model FiniteAutomaton RegularExpression			\
			ContextFreeGrammar PushdownAutomaton RDParser LL1Grammar		\
			LRGrammar ContextFreeGrammarFull FiniteEnumeration PolyModel	\
			TopLevel LearnOCaml Tests PreOpen

define SRC_FILES
	$(addprefix $(SRC_DIR)/, $(addsuffix .ml, $(foreach file, $(NAMES), $(file))))
endef

$(LIB).ml: $(SRC_FILES)
	@echo "TYPE CHECKING: *.ml"
	@$(shell ocaml $(SERVICE) make $(NAMES))
	@cd src ; rm -f *.cmi *.cmo
	@echo "GENERATING:" $(LIB).ml
	@mkdir -p lib
	@cat $(SRC_FILES) > $(LIB).ml

$(LIB).cma: $(LIB).ml
	@echo "COMPILING:" $(LIB).cma
	@$(COMP) $(FLAGS) -o $(LIB).cma -a $(LIB).ml
	@rm -f $(LIB).cmi $(LIB).cmo

.PHONY: all
all: $(LIB).cma

.PHONY: run
run: $(LIB).ml
	@echo "RUNNING:" $(LIB).ml
	@rlwrap ocaml -init $(LIB).ml

.PHONY: test
test: $(LIB).ml
	@echo "RUNNING TESTS..."
	@TESTING=true ocaml $(LIB).ml

.PHONY: types
types: $(LIB).ml
	echo "#use \"lib/OCamlFlat.ml\";;" | ocaml;

clean:
	@chmod 600 $(SRC_FILES) $(SERVICE)
	rm -rf $(LIB_DIR) $(SRC_DIR)/*.cmi $(SRC_DIR)/*.cmo
	
.PHONY: edit
edit:
	@geany -s OCamlFlat.geany src/*.ml $(shell find ../OFLAT/src -name *.ml ! -name 'OCamlFlat.ml')

.PHONY: dist
dist: $(LIB_DIR)
	rm -rf OCamlFlat
	mkdir OCamlFlat
	cp -a LICENCE Makefile README.md OCamlFlat.geany src OCamlFlat
	tar cpvz OCamlFlat > OCamlFlat.tgz
	rm -rf OCamlFlat

.PHONY: git0
git0:
	git fetch
	git status

.PHONY: git1
git1:
	git fetch
	git diff

.PHONY: git2
git2:
	git fetch
	git add src/*.ml Makefile OCamlFlat.geany
	git status

.PHONY: git3
git3:
	git fetch
	git commit
	git status

.PHONY: git4
git4:
	git push origin master
