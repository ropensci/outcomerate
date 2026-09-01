# outcomerate 1.1.0

* Updated the outcome-rate formulas, documentation, and bibliography to the
  AAPOR *Standard Definitions*, 10th edition (2023).
* Added support for `UR`, the 10th edition's new aggregate symbol for cases
  where it is unknown whether the sampled unit is eligible or the housing unit
  contains an eligible respondent (code 3.20). The underlying 3.20 disposition
  is not new: the 9th edition included it in `UO`. Existing `UO` inputs and
  results remain supported, while newly coded 3.20 cases should use `UR`.
* Aligned weighted-rate guidance with the 10th edition: individual zero weights
  are accepted for two-phase designs, while invalid and all-zero weight vectors
  are rejected. Already-aggregated inputs now reject `weight` instead of
  silently returning results labeled as weighted.
* `outcomerate()` now accepts category-specific eligibility estimates such as
  `e = c(UH = 0.4, UR = 0.7, UO = 0.2)`. Existing scalar `e` inputs retain
  their original behavior.
* Aggregate disposition counts now reject duplicate names rather than silently
  discarding later values during canonical ordering.
* Minor updates to vignettes to reflect changes in the tidyverse.
* Updated legacy citation, documentation, and test attribute syntax for current
  R and CRAN checks.

# outcomerate 1.0.1

#### Documentation

* Added CITATION details to the package
* Added documentation as `pkgdown` site

# outcomerate 1.0.0

#### New Features

* `eligibility_rate()` function added to estimate the proportion of eligible cases from the unknowns, based on the known ineligibles (`NE`'s).

#### Improvements

* Refactoring of code based on ROpenSci peer review feedback.
* Added S3 method for factors.
* Addition of many more unit tests.
* Additional of more helpful error messages.

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
