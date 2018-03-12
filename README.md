The Savitch Variations
----------------------

We present variations around the theme of Savitch Theorem.

Variation 0 -- Non-deterministic Polynomial Space algorithm
===========================================================

This variation shows a non-deterministic polynomial space
algorithm to decide the equivalence of two nfas.
This variation is numbered zero because it is root of all the other variations illustrating Savitch Theorem.


Remark: This first variation, although completely impractical, shows
an interesting property. The test gets stuck when it encounters a pair
of equivalent nfas. Why does it get stuck here? Simply because
according to the algorithm, it has to try all the paths of length less
than 2^(n1+n2), which means 2^(2^(n1+n2)) steps.

Variation 1 -- The Theme -- Deterministic Polynomial space
==========================================================

Coming soon