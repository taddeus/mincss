RESULT    := mincss
PRE_TGTS  := types
MODULES   := color_names util stringify parser lexer parse simple shorthand \
             duplicates main
ALL_NAMES := $(PRE_TGTS) $(MODULES)

OCAMLCFLAGS  := -g
OCAMLLDFLAGS :=
OCAMLLDLIBS  := str.cmxa

OCAMLLEX  := ocamllex
OCAMLYACC := menhir --infer --explain --dump
OCAMLOPT_GETOPT := ocamlfind opt -linkpkg -package getopt

.PHONY: all clean
.PRECIOUS: $(addprefix .cmi,$(ALL_NAMES))

all: $(RESULT)

%.ml: %.mll
	$(OCAMLLEX) -o $@ $<

%.ml: %.mly
	$(OCAMLYACC) $<

%.cmi: %.mli
	ocamlc -c $(OCAMLCFLAGS) -o $@ $<

%.cmx: %.ml
	ocamlopt -c $(OCAMLCFLAGS) -o $@ $(<:.cmi=.ml)

main.cmx: main.ml
	$(OCAMLOPT_GETOPT) -c $(OCAMLCFLAGS) -o $@ $(<:.cmi=.ml)

$(RESULT): $(addsuffix .cmx,$(ALL_NAMES))
	$(OCAMLOPT_GETOPT) -o $@ $(OCAMLLDFLAGS) $(OCAMLLDLIBS) $^

# module dependencies
lexer.cmi: lexer.ml
parser.cmx: parser.cmi lexer.cmx
parser.mli: parser.ml
parse.cmx: lexer.cmi parser.cmx
main.cmx: parse.cmx util.cmx simple.cmx shorthand.cmx duplicates.cmx
util.cmx: OCAMLCFLAGS += -pp cpp
util.cmx simple.cmx: color_names.cmx
stringify.cmx parser.cmx simple.cmx shorthand.cmx: util.cmi
$(addsuffix .cmx,$(MODULES)): $(addsuffix .cmi,$(PRE_TGTS))

%.html: %.md
	markdown $^ > $@

clean:
	rm -f *.cmi *.cmx *.o lexer.ml parser.ml parser.mli parser.conflicts \
		parser.automaton README.html $(RESULT)
