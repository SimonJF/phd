OCB_FLAGS = -use-ocamlfind -I src -I lib

OCB = 		ocamlbuild $(OCB_FLAGS)

all:	native byte

clean:
	$(OCB) -clean

native: 	sanity
	$(OCB) main.native

byte:  	sanity
	$(OCB) main.byte

debug: sanity
	$(OCB) -tag debug main.byte

sanity:
	which ocamlfind

test: native
	./main.native

.PHONY:
	all clean byte native profile debug sanity test

