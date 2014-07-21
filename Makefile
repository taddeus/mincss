RESULT    := mincss
PRE_TGTS  := types
MODULES   := util stringify parser lexer parse color main
ALL_NAMES := $(PRE_TGTS) $(MODULES)

OCAMLCFLAGS  := -g
OCAMLLDFLAGS :=
OCAMLLDLIBS  := str.cmxa

OCAMLLEX  := ocamllex
OCAMLYACC := menhir --infer --explain --dump

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
	ocamlfind ocamlopt -package batteries -c $(OCAMLCFLAGS) -o $@ $(<:.cmi=.ml)

$(RESULT): $(addsuffix .cmx,$(ALL_NAMES))
	ocamlopt -o $@ $(OCAMLLDFLAGS) $(OCAMLLDLIBS) $^

# intra-module dependencies
lexer.cmi: lexer.ml
parser.cmx: parser.cmi lexer.cmi
parser.mli: parser.ml
parse.cmx: lexer.cmi parser.cmx
main.cmx: parse.cmx util.cmx color.cmx
$(addsuffix .cmx,$(MODULES)): $(addsuffix .cmi,$(PRE_TGTS))

clean:
	rm -f *.cmi *.cmx *.o lexer.ml parser.ml parser.mli parser.conflicts \
		parser.automaton $(RESULT)
