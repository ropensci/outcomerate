# outcomerate 0.0.1.9000

#### New Features

* `eligibility_rate()` function added to estimate the proportion of eligible cases from the unknowns, based on the known ineligibles (`NE`'s).

#### Improvements

* Refactoring of code based on ROpenSci peer review feedback.
* Added S3 method for factors.
* Addition of many more unit tests.
* Addtional of more helpful error messages.

#### Breaking Changes

* `weight` argument no longer accepts scalar inputs.
* If weights are provided, the output labels are renamed in the form 'RR2w' instead of "RR2"
* If `rate = NULL` in the function parameters, the default behavior will be to return all possible rates given the other parameters specified.
* Disposition codes now accept "NE" for known ineligibles. Within `outcomerate()`, these are largely ignored, but are used by `eligibility_rate()` to estimate `e`

#### Documentation

* Added documentation for the (internal) `fmat` formula matrix object
* Added documentation on the `middleearth` toy dataset

# outcomerate 0.0.0.9000

* Created `outcomerate` package
