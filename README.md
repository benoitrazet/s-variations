Savitch Variations
==================

This project is about variations around the central theme of Savitch's Theorem.

Variation 0 -- Non-deterministic Polynomial Space algorithm
-----------------------------------------------------------

This variation shows a non-deterministic polynomial space algorithm to
decide the equivalence of two NFAs.  This variation is numbered zero
because it is the root of the next variations.


Remark: This first variation demonstrates an interesting behavior
right away. The test gets stuck when it encounters a pair of NFAs that
are equivalent. Why is it getting stuck when 2 NFAs are equivalent?
Simply because, according to the algorithm, it non-deterministically
tries all the paths of length less than 2^(n1+n2), which means
2^(2^(n1+n2)) steps.

Variation 1 -- Savitch's Theorem
--------------------------------

This variation demonstrates the most direct implementation based on
Savitch's Theorem to deciding the equivalence of NFAs.

The complexity of this algorithm for the best-case and the worst-case is
2^{(n1+n2)^2}. If n1 and n2 are both equal to 3, it results in 2^36 ~= 64G
steps.

Variation 2 -- Counting and Immerman-Szelepcsényi Theorem
---------------------------------------------------------

This variation demonstrates how counting can improve widely the
complexity of the naive implementation, deciding the equivalence of
NFAs in polynomial space.  The same idea is
used in the Immerman-Szelepcsényi Theorem which establishes the
equivalence between NSPACE(S(n)) and co-NSPACE(S(n)).


Variation 3 -- Smart guessing the middle configurations
-------------------------------------------------------

This variation proposes to improve the running time by upgrading the
method of enumeration of the middle configurations. In the previous
variations, the enumeration of the middle configurations is completely
naive and enumerates all the possible configurations (the best case is
the worst case again). To illustrate how impractical it is, consider
the case when there is a set of states is are not reachable from a
starting configuration, then all the combinations of this states would
be generated with no chance of success. In this variation we developed
a technique to enumerate only the middle configurations that have a
chance to be successful. The main idea is to consider the matrix
associated with each NFAs where the element a_{i,j} is an interval
[m,n] indicating that it is possible to go from state i to state j
with a string of length least m and at most n.

The smart enumeration remains in the polynomial space complexity and
therefore the whole algorithm remains in DSPACE(n^2).



Variation 4 -- Dynamic Programming
----------------------------------

Coming Soon