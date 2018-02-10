hmatrix-backprop
================

*[hmatrix][]* operations lifted for *[backprop][]*.

[hmatrix]: http://hackage.haskell.org/package/hmatrix
[backprop]: http://hackage.haskell.org/package/backprop

Meant to act as a drop-in replacement to the API of
[Numeric.LinearAlgebra.Static][static].  Just change your imports, and your
functions are automatically backpropagatable.  Useful types are all
re-exported.

[static]: https://hackage.haskell.org/package/hmatrix-0.18.2.0/docs/Numeric-LinearAlgebra-Static.html

Formulas for gradients come from the following papers:

*   <https://people.maths.ox.ac.uk/gilesm/files/NA-08-01.pdf>
*   <http://www.dtic.mil/dtic/tr/fulltext/u2/624426.pdf>
*   <http://www.cs.cmu.edu/~zkolter/course/15-884/linalg-review.pdf>
*   <https://arxiv.org/abs/1602.07527>

Some functions are not yet implemented!  See module documentation for details.
PR's definitely appreciated :)

Tests
-----

Currently numeric tests are implemented as property tests using hedgehog, but
it is possible that the answers might differ from the true values by an amount
undetectable by property tests.

All functions currently are tested except for the higher-order functions.

They are tested by "nudging" components of inputs and checking if the change in
the function outputs match what is expected from the backpropagated gradient.

TODO
----

Apart from the exact API of hmatrix, it'd be nice to have:

1.  Statically sized convolutions.  Should probably add this to hmatrix instead
    first, though.


