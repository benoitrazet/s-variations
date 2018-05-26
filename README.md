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


Variation 3 -- Accessibility Matrix as Transitive Closure
---------------------------------------------------------

Coming soon


Variation 4 -- Dynamic Programming
----------------------------------

Coming eventually