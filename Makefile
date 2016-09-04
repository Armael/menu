all:
	ocamlbuild -use-ocamlfind menu.native

clean:
	ocamlbuild -clean
