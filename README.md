Savitch Variations
==================

This project is about variations around the central theme of Savitch's Theorem.

Savitch's Theorem shows that any problem that can be solved with a
non-deterministic machine in space $S(n)$, where $n$ is the size of
the input, can also be solved with a deterministic machine in space
$S(n)^2$, in other words $NSPACE(S(n)) \subseteq DSPACE(S(n)^2)$; a
corollary of this result for polynomial space algorithms is the
equivalence $PSPACE = NPSPACE$. This is the basis to the claim that
non-determinism does not extend the power of machines running with
space restrictions. While that may be the case for the large class of
polynomial space problems, it seems less clear for finer classes like
$NSPACE(n)$, which is the class of problems that can be solved with a
non-deterministic machine in linear space. Let us recall that the
1st LBA conjecture is still open, $NSPACE(n) vs DSPACE(n)$, and that
the second LBA conjecture has been solved establishing the
equivalence $NPSPACE(n) = coNSPACE(n)$.

We propose to study the Savitch's Theorem applied to one of the most
representative problem for $coNSPACE(n)$ problem, which is the Equivalence
of NFAs.

Variation 0 -- Non-deterministic Polynomial Space Algorithm
-----------------------------------------------------------

This variation shows a non-deterministic polynomial space algorithm to
decide the equivalence of two NFAs and simulates it with a naive
deterministic algorithm. This variation is numbered zero because it
is the root of the next variations.


Remark: This first variation demonstrates an interesting behavior
right away. The test gets stuck when it encounters a pair of NFAs that
are equivalent. Why is it getting stuck when 2 NFAs are equivalent?
Simply because, according to the algorithm, it non-deterministically
tries all the paths of length less than $2^(n1+n2)$, which means
2^(2^(n1+n2)) steps.

Variation 1 -- Savitch's Theorem
--------------------------------

This variation demonstrates the most direct implementation based on
Savitch's Theorem for deciding the equivalence of NFAs.

The complexity of this algorithm for the best-case and the worst-case
is 2^{(n1+n2)^2}. If n1 and n2 are both equal to 3, it results in 2^36
~= 64G steps.

Variation 2 -- Counting and Immerman-Szelepcsényi Theorem
---------------------------------------------------------

This variation demonstrates how counting can improve widely the
complexity of the naive implementation, deciding the equivalence of
NFAs in polynomial space.  The same idea is used in the
Immerman-Szelepcsényi Theorem which establishes the equivalence
between NSPACE(S(n)) and co-NSPACE(S(n)).


Variation 3 -- Narrowing and Ordering Enumerations
--------------------------------------------------

This variation improves the running time by refining the enumeration
of the middle configurations. In the previous variations, the
enumeration of the middle configurations is completely naive and
enumerates all the possible configurations -- the best case meets the
worst case. To illustrate how impractical it is, consider some states
that is are not reachable from a starting configuration, then all the
combinations of these states would be enumerated despite the
impossibility to reach these configurations.

In this variation we developed a technique to narrow the enumeration
of the middle configurations based on the specific inputs and also
reorder the enumeration with an heuristics to maximize the
reachability. The main idea is to consider the matrix associated with
each NFAs where the element a_{i,j} is an interval [m,n] indicating
that it is possible to go from state i to state j with a string of
length at least m and at most n.

This enumeration remains in polynomial space complexity and
therefore the whole algorithm remains in DSPACE(n^2).



Variation 4 -- Dynamic Programming
----------------------------------

Coming Soon