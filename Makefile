RESULT    := mincss
BASENAMES := types stringify parser lexer util parse main
OFILES    := $(addsuffix .cmx,$(BASENAMES))

OCAMLCFLAGS  := -g
OCAMLLDFLAGS :=
OCAMLLDLIBS  := str.cmxa

OCAMLLEX  := ocamllex
OCAMLYACC := menhir --infer --explain --dump

.PHONY: all clean
.PRECIOUS: $(addprefix .cmi,$(BASENAMES))

all: $(RESULT)

%.ml: %.mll
	$(OCAMLLEX) -o $@ $<

%.ml: %.mly
	$(OCAMLYACC) $<

%.cmi: %.mli
	ocamlc -c $(OCAMLCFLAGS) -o $@ $<

%.cmx: %.ml
	ocamlfind ocamlopt -package batteries -c $(OCAMLCFLAGS) -o $@ $(<:.cmi=.ml)

$(RESULT): $(OFILES)
	ocamlopt -o $@ $(OCAMLLDFLAGS) $(OCAMLLDLIBS) $^

# intra-module dependencies
lexer.cmi: lexer.ml
parser.cmx: parser.cmi lexer.cmi
parser.mli: parser.ml
parse.cmx: lexer.cmi parser.cmx
util.cmx: stringify.cmx
main.cmx: parse.cmx util.cmx
stringify.cmx parser.cmi parser.cmx lexer.cmx util.cmx parse.cmx main.cmx: \
	types.cmi

clean:
	rm -f *.cmi *.cmx *.o lexer.ml parser.ml parser.mli parser.conflicts \
		parser.automaton $(RESULT)
