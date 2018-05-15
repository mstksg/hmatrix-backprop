Changelog
=========

Version 0.1.2.2
---------------

*Unreleased*

<https://github.com/mstksg/hmatrix-backprop/releases/tag/v0.1.2.2>

*   Rewrote most of *Numeric.LinearAlgebra.Static.Backprop* module to require
    `Backprop` constraints on everything instead of `Num` constraints
*   Re-ordered constraint orders on various functions.  *Potentially breaking
    change* if TypeApplications are used.

Version 0.1.2.1
---------------

*May 1, 2018*

<https://github.com/mstksg/hmatrix-backprop/releases/tag/v0.1.2.1>

*   Updated for compatibility with *backprop-0.2.0.0*
*   Orphan instances added for types in *Numeric.LinearAlgebra.Static*.

Version 0.1.2.0
---------------

*Mar 30, 2018*

<https://github.com/mstksg/hmatrix-backprop/releases/tag/v0.1.2.0>

*   `zipWithVector` family rewritten in the same way as `dvmap`/`dvmap` were
    for version 0.1.1.0, which is a breaking API change.  However, again, it is
    unlikely that any code using the previous versions compiled at all, so in
    practicality, no actual code that previously worked is now breaking.

Version 0.1.1.0
---------------

*Mar 25, 2018*

<https://github.com/mstksg/hmatrix-backprop/releases/tag/v0.1.1.0>

*   `dvmap`/`dmmap` family rewritten, which is a breaking API change.  Previous
    version of `dvmap`/`dmmap` would not even compile at all if used, though,
    because of nonsensical constraints, so it is likely that no code that
    previously worked is now breaking.
*   *backprop* types re-exported for convenience

*Internal*

*   Rewrote much code to use `isoVar` instead of `opIso`, for increased clarity
    and cleanliness.  Requires *backprop-0.1.4.0*.

Version 0.1.0.0
---------------

*Feb 10, 2018*

<https://github.com/mstksg/hmatrix-backprop/releases/tag/v0.1.0.0>

*   Initial release
