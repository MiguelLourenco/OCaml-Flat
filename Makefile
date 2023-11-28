VERSION = 1.0
COMP = ocamlc
FLAGS =
LIB_DIR = lib
LIB_NAME = OCamlFlat
LIB = $(LIB_DIR)/$(LIB_NAME)
SRC_DIR = src
SERVICE = $(SRC_DIR)/Service.ml
NAMES =		OCamlFlat Configuration Error 			\
													\
			Set BasicTypes Util						\
													\
			Scanner JSon							\
													\
			Examples Entity Exercise Model 			\
													\
			FinAutSyntax FiniteAutomaton			\
													\
			RegExpSyntax RegularExpression			\
													\
			FinEnuSyntax FiniteEnumeration			\
													\
			CFGSyntax CFGChomsky ContextFreeGrammar	\
			RDParser LL1Grammar						\
			LRGrammar								\
			ContextFreeGrammarFull					\
													\
			TuringMachine  							\
													\
			PushdownAutomaton	PolyModel			\
													\
													\
			TopLevel LearnOCaml Tests PreOpen

define SRC_FILES
	$(addprefix $(SRC_DIR)/, $(addsuffix .ml, $(foreach file, $(NAMES), $(file))))
endef

$(LIB).ml: $(SRC_FILES)
	@echo "TYPE CHECKING: *.ml"
	@$(shell ocaml $(SERVICE) make $(NAMES)) || (rm -f *.cm[io] ; exit 1)
	@rm -f $(SRC_DIR)/*.cm[io]
	@echo "GENERATING:" $(LIB).ml
	@mkdir -p lib
	@cppo -D ALL $(SRC_FILES) > $(LIB).ml
	@ls -l $(LIB).ml

.PHONY: small
small: $(SRC_FILES)
	@echo "TYPE CHECKING: *.ml"
	@$(shell ocaml $(SERVICE) make_small $(NAMES)) || (rm -f *.cm[io] ; exit 1)
	@rm -f $(SRC_DIR)/*.cm[io]
	@echo "GENERATING:" $(LIB).ml
	@mkdir -p lib
	@cppo $(SRC_FILES) > $(LIB).ml
	@ls -l $(LIB).ml

$(LIB).cma: $(LIB).ml
	@echo "COMPILING:" $(LIB).cma
	@$(COMP) $(FLAGS) -o $(LIB).cma -a $(LIB).ml
	@rm -f $(LIB).cmi $(LIB).cmo

.PHONY: tags
tags:
	find -L src -not -path '*/\.*' \( -name "*.ml" \)			\
	| ctags --totals --fields=fKsSt --extra=-fq --c-kinds=+p	\
			--sort=foldcase --excmd=number -L - -f $(LIB_NAME).tags

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
	echo "#use \"lib/$(LIB_NAME).ml\";;" | ocaml;

.PHONY: clean
clean:
	@chmod 600 $(SRC_FILES) $(SERVICE)
	rm -rf $(LIB_DIR) $(SRC_DIR)/*.cmi $(SRC_DIR)/*.cmo

.PHONY: edit
edit:
	@cd $(SRC_DIR) ; geany ../$(LIB_NAME).geany &

.PHONY: editreset
editreset:
	cp geany.template $(LIB_NAME).geany
	sed -i -e "s/PROJECT_NAME/$(LIB_NAME)/" $(LIB_NAME).geany
	@echo PLEASE, activate the plugins "File Browser" and "Split Window"; sleep 3
	$(MAKE) edit

.PHONY: dist
dist: clean
	rm -rf $(LIB_NAME); mkdir $(LIB_NAME)
	cp -a LICENCE Makefile README.md $(LIB_NAME).geany src $(LIB_NAME)
	tar cpvz $(LIB_NAME) > $(LIB_NAME).tgz
	rm -rf $(LIB_NAME)

.PHONY: git0
git0: clean
	git fetch
	git status

.PHONY: git1
git1: clean
	git fetch
	git diff

.PHONY: git2
git2: clean
	git fetch
	git add src/*.ml Makefile geany.template
	git status

.PHONY: git3
git3: clean
	git fetch
	git commit
	git status

.PHONY: git4
git4:
	git push origin master
