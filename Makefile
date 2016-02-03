RESULT    := mincss
PRE_TGTS  := types
MODULES   := color_names util stringify parser lexer parse simple shorthand \
             duplicates main
ALL_NAMES := $(PRE_TGTS) $(MODULES)

OCAMLCFLAGS  := -g
RESULTLDLIBS = getopt str
OCAMLLDFLAGS = $(if $(OCAMLLDLIBS),-linkpkg) \
			   $(addprefix -package ,$(OCAMLLDLIBS))

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
	ocamlfind opt $(OCAMLLDFLAGS) -c $(OCAMLCFLAGS) -o $@ $(<:.cmi=.ml)

$(RESULT): $(addsuffix .cmx,$(ALL_NAMES))
	ocamlfind opt -linkpkg $(addprefix -package ,$(RESULTLDLIBS)) -o $@ $^

# module dependencies
lexer.cmi: lexer.ml
parser.cmx: parser.cmi lexer.cmx
parser.mli: parser.ml
parse.cmx: lexer.cmi parser.cmx
main.cmx: OCAMLLDLIBS=getopt
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
