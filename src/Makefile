VERSION=0.1

OCAMLC=ocamloptp -P a

SOURCES=utils.ml set.ml nfa.ml variation0_npspace.ml variation1_savitch_theorem.ml test.ml

EXECUTABLES=test.out

DOC=NOTES

all: test

test:
	$(OCAMLC) -c utils.ml
	$(OCAMLC) -c set.mli
	$(OCAMLC) -c set.ml
	$(OCAMLC) -c nfa.ml
	$(OCAMLC) -c variation0_npspace.ml
	$(OCAMLC) -c variation1_savitch_theorem.ml
	$(OCAMLC) -c variation2_counting.ml
	$(OCAMLC) -c transitive_closure.ml
	$(OCAMLC) -c test_transitive_closure.ml
	$(OCAMLC) transitive_closure.cmx test_transitive_closure.cmx -o test_transitive_closure.out
	$(OCAMLC) -c variation3.ml
	$(OCAMLC) -c npspace_eq_memo.ml
	$(OCAMLC) -c test_correctness.ml
	$(OCAMLC) utils.cmx set.cmx nfa.cmx variation0_npspace.cmx variation1_savitch_theorem.cmx variation2_counting.cmx transitive_closure.cmx variation3.cmx npspace_eq_memo.cmx test_correctness.cmx -o test_correctness.out
	$(OCAMLC) -c test.ml
	$(OCAMLC) utils.cmx set.cmx nfa.cmx variation0_npspace.cmx variation1_savitch_theorem.cmx variation2_counting.cmx transitive_closure.cmx variation3.cmx test.cmx -o test.out

clean:
	rm *~ *.cmx *.cmi *.o

version:
	mkdir SVARIATIONS_$(VERSION); cd SVARIATIONS_$(VERSION)
	cp $(SOURCES) Makefile $(EXECUTABLES) $(DOC) SVARIATIONS_$(VERSION)/.

