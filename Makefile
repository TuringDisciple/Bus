all: parser.cmi combinators.cmi combinators.cmo example.cmo a.out

parser.cmi: parser.ml
	ocamlc -o parser parser.mli

combinators.cmi: combinators.mli
	ocamlc -o combinators combinators.mli

combinators.cmo: parser.ml combinators.ml
	ocamlc -c  combinators.ml

example.cmo: example.ml parser.ml combinators.ml
	ocamlc -c example.ml

a.out: parser.ml combinators.ml example.ml
	ocamlc $^