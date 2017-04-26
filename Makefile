all:
	ocamlfind ocamlc -g -dtypes -linkpkg -syntax camlp4o -package camlp4.extend -package camlp4.lib -package llvm -package llvm.bitwriter minml.ml -o minml
