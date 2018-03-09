VERSION=0.1

SOURCES=utils.ml set.ml nfa.ml variation0_npspace.ml test.ml

EXECUTABLES=test.out

DOC=NOTES

all: test

test:
	ocamlopt -c utils.ml
	ocamlopt -c set.ml
	ocamlopt -c nfa.ml
	ocamlopt -c variation0_npspace.ml
	ocamlopt -c test.ml
	ocamlopt utils.cmx set.cmx nfa.cmx variation0_npspace.cmx test.cmx -o test.out

clean:
	rm *.cmx *.cmi *.o

version:
	mkdir SVARIATIONS_$(VERSION); cd SVARIATIONS_$(VERSION)
	cp $(SOURCES) Makefile $(EXECUTABLES) $(DOC) SVARIATIONS_$(VERSION)/.

