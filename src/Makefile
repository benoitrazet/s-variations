VERSION=0.1

SOURCES=utils.ml set.ml nfa.ml variation0_npspace.ml variation1_savitch_theorem.ml test.ml

EXECUTABLES=test.out

DOC=NOTES

all: test

test:
	ocamlopt -c utils.ml
	ocamlopt -c set.mli
	ocamlopt -c set.ml
	ocamlopt -c nfa.ml
	ocamlopt -c variation0_npspace.ml
	ocamlopt -c variation1_savitch_theorem.ml
	ocamlopt -c variation2_counting.ml
	ocamlopt -c transitive_closure.ml
	ocamlopt -c test_transitive_closure.ml
	ocamlopt transitive_closure.cmx test_transitive_closure.cmx -o test_transitive_closure.out
	ocamlopt -c variation3.ml
	ocamlopt -c npspace_eq_memo.ml
	ocamlopt -c test_correctness.ml
	ocamlopt utils.cmx set.cmx nfa.cmx variation0_npspace.cmx variation1_savitch_theorem.cmx variation2_counting.cmx transitive_closure.cmx variation3.cmx npspace_eq_memo.cmx test_correctness.cmx -o test_correctness.out
	ocamlopt -c test.ml
	ocamlopt utils.cmx set.cmx nfa.cmx variation0_npspace.cmx variation1_savitch_theorem.cmx variation2_counting.cmx transitive_closure.cmx variation3.cmx test.cmx -o test.out

clean:
	rm *~ *.cmx *.cmi *.o

version:
	mkdir SVARIATIONS_$(VERSION); cd SVARIATIONS_$(VERSION)
	cp $(SOURCES) Makefile $(EXECUTABLES) $(DOC) SVARIATIONS_$(VERSION)/.

