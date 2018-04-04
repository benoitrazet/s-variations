The Savitch Variations
======================

We present variations around the theme of Savitch's Theorem.

Variation 0 -- Non-deterministic Polynomial Space algorithm
-----------------------------------------------------------

This variation shows a non-deterministic polynomial space algorithm to
decide the equivalence of two nfas.  This variation is numbered zero
because it is root of all the other variations illustrating Savitch
Theorem.


Remark: This first variation, although completely impractical, shows
an interesting property. The test gets stuck when it encounters a pair
of equivalent nfas. Why is it getting stuck? Simply because
according to the algorithm, it tries all the paths of length less
than 2^(n1+n2), which means 2^(2^(n1+n2)) steps.

Variation 1 -- Savitch Theorem
------------------------------

This is the most naive implementation applying Savitch's Theorem to
deciding the equivalence of nfas.

The complexity of this algorithm is best and worst-case in
2^{(n1+n2)^2}. If n1 and n2 are both equal to 3, that makes 2^36
steps.

Variation 2 -- Counting and Immerman-Szelepcsényi Theorem
---------------------------------------------------------

This variation demonstrates how counting can improve widely the
complexity of deciding the equivalence of nfas in polynomial space.  I
discovered that the same technique is used in the
Immerman-Szelepcsényi Theorem which establishes the equivalence of
NSPACE(S(n)) and co-NSPACE(S(n)).