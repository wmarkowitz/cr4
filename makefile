cr4: cr4.ml
	ocamlbuild -use-ocamlfind cr4.byte

cr4_solutions: cr4_solutions.ml
	ocamlbuild -use-ocamlfind cr4_solutions.byte

all: cr4

clean:
	rm -rf _build *.byte