all:
	ocamlbuild -use-ocamlfind minml.native

opt:
	ocamlbuild -use-ocamlfind minml.byte

clean:
	ocamlbuild -clean

ocamlfind:
	ocamlfind ocamlc -g -dtypes -linkpkg -syntax camlp4o -package camlp4.extend -package camlp4.lib -package llvm -package llvm.bitwriter minml.ml -o minml

.PHONY: all opt clean ocamlfind
