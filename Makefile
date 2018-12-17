all: parser.cmi combinators.cmo example.cmo

parser.cmi: parser.ml
	ocamlc -o parser parser.mli

combinators.cmo: parser.ml combinators.ml
	ocamlc -c  combinators.ml

example.cmo: example.ml parser.ml combinators.ml
	ocamlc -c example.ml