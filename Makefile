RESULT := mincss
BASENAMES := types stringify parser lexer util parse main

OFILES := $(addsuffix .cmx,$(BASENAMES))

OCAMLCFLAGS := -g
OCAMLLDFLAGS :=
OCAMLLDLIBS := str.cmxa

.PHONY: all clean
.PRECIOUS: $(addprefix .cmi,$(BASENAMES))

all: $(RESULT)

%.ml: %.mll
	ocamllex -o $@ $<

%.ml: %.mly
	menhir --infer --explain $<

%.cmi: %.mli
	ocamlc -c $(OCAMLCFLAGS) -o $@ $<

parser.cmx: parser.cmi
parser.mli: parser.ml

%.cmx: %.ml
	ocamlfind ocamlopt -package batteries -c $(OCAMLCFLAGS) -o $@ $(<:.cmi=.ml)

$(RESULT): $(OFILES)
	ocamlopt -o $@ $(OCAMLLDFLAGS) $(OCAMLLDLIBS) $^

clean:
	rm -f *.cmi *.cmx *.o lexer.ml parser.ml parser.mli $(RESULT)
