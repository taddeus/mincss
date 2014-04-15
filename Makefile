RESULT := mincss
SOURCES := types.ml types.mli stringify.ml lexer.mll parser.mly util.ml parse.ml \
	main.ml
PRE_TARGETS := types.ml types.cmi stringify.cmi
LIBS := str

# Set debugging flag to enable exception backtraces for OCAMLRUNPARAM=b
OCAMLFLAGS := -g

OCAMLYACC := menhir
YFLAGS := --infer --explain --dump

.PHONY: all myclean

all: native-code

clean:: myclean

# The Types module needs an implementation to stop ocamlc from complaining
types.ml: types.mli
	cp $< $@

myclean:
	rm -f a.out types.ml parser.conflicts parser.automaton

include OCamlMakefile
