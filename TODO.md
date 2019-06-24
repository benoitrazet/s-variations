- (2019-06-23) Replace all the print with to_string and logging

- (2019-06-23) Use and upgrade engine for variation 2. The engine
  already has function to support it.

- (2018-08-07) create an interface for streams and enumerations. This
  should provide cleaner evidence that algo is in PSPACE.

- (2018-06-03) ~~Need to clean up the interface for set and
  subsets. No more exposition of the data-structure that can be used
  at a low level in othe modules. That's clear after this subtle
  omission of normalization of set in yield1 function.~~ (2018-06-15)

- (2018-06-02) In the savitch engine that count the number of
  solutions, remove the use of the bound (2^{n1+n2}) because the
  fixpoint will be reached before reaching the bound. Remove `bound`
  in variation2 and revise variation3.

- ~~(2018-05-31) Add correctness test for different versions~~
  (2018-06-03)

- (2018-05-26) Have comprehensive testing for all the variations

- (2018-05-26) Use a logger

- (2018-05-26) Generalize the search algorithm into an engine
  parametrized by an emuration

- ~~(2018-05-26) add testing to modules (transitive closure). Tests
  would show examples and allow to catch regression~~ (2018-05-29)
