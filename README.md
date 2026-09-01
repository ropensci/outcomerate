
<!-- badges: start -->

[![R build
status](https://github.com/ropensci/outcomerate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/outcomerate/actions/workflows/R-CMD-check.yaml)
[![pkgcheck](https://github.com/ropensci/outcomerate/actions/workflows/pkgcheck.yaml/badge.svg)](https://github.com/ropensci/outcomerate/actions/workflows/pkgcheck.yaml)
[![Coverage
status](https://codecov.io/gh/ropensci/outcomerate/branch/main/graph/badge.svg)](https://app.codecov.io/github/ropensci/outcomerate?branch=main)
[![Ropensci
status](https://badges.ropensci.org/213_status.svg)](https://github.com/ropensci/software-review/issues/213)
[![CRAN
status](https://www.r-pkg.org/badges/version/outcomerate)](https://CRAN.R-project.org/package=outcomerate)
<!-- badges: end -->

# outcomerate

`outcomerate` is a lightweight R package that implements the standard
outcome rates for surveys, as defined in the [Standard Definitions, 10th
edition](https://aapor.org/wp-content/uploads/2024/03/Standards-Definitions-10th-edition.pdf)
of the American Association for Public Opinion Research (AAPOR).

Although the mathematical formulas are straightforward, it can get
tedious and repetitive calculating all the rates by hand, especially for
sub-groups of your study. The formulas are similar to one another and so
it is also dangerously easy to make a clerical mistake. The
`outcomerate` package simplifies the analytical workflow by defining all
formulas as a collection of functions.

The 10th edition separates code 3.20 from `UO` under the aggregate
symbol `UR`. Legacy `UO` data remain supported and produce the same
rates when the package’s scalar eligibility estimate `e` is used; newly
coded 3.20 cases should use `UR`.

## Installation

Install the package from CRAN:

``` r
install.packages("outcomerate")
```

Alternatively, install the latest development version via github:

``` r
#install.packages("devtools")
devtools::install_github("ropensci/outcomerate")
```

## Example

Let’s say you draw a sample of 13 cases. After finishing the fieldwork,
you tabulate all your attempts into a table of disposition outcomes:

| code | disposition | n |
|:---|:---|---:|
| I | Complete interview | 4 |
| P | Partial interview | 2 |
| R | Refusal and break-off | 1 |
| NC | Non-contact | 1 |
| O | Other | 1 |
| UH | Unknown if household | 1 |
| UR | Unknown if sampled unit is eligible / housing unit contains an eligible respondent | 1 |
| NE | Not eligible | 1 |
| UO | Unknown, other | 1 |

Using this table, you may wish to report some of the common survey
outcome rates, such as:

- **Response Rate:** The proportion of your sample that results in an
  interview.
- **Cooperation Rate:** The proportion of eligible units ever contacted
  that result in an interview; the package implements AAPOR’s
  household-level formulas.
- **Refusal Rate:** The proportion of your sample that refused to
  participate.
- **Contact Rate:** The proportion of sampled cases where a responsible
  member of the housing unit is reached; the package implements AAPOR’s
  household-level formulas.
- **Location Rate:** The proportion of cases that you manage to locate.
  This is a package extension, not an AAPOR-defined rate, and its
  interpretation depends on how located cases are coded.

Most of these rates come under a number of variants, having definitions
that are standardized by AAPOR. The `outcomerate` function lets you
calculate these rates seamlessly:

``` r
# load package
library(outcomerate)

# set counts per disposition code (needs to be a named vector)
freq <- c(I = 4, P = 2, R = 1, NC = 1, O = 1,
          UH = 1, UR = 1, UO = 1, NE = 1)

# calculate rates, assuming 90% of unknown cases are eligible
outcomerate(freq, e = eligibility_rate(freq))
#>   RR1   RR2   RR3   RR4   RR5   RR6 COOP1 COOP2 COOP3 COOP4  REF1  REF2  REF3 
#> 0.333 0.500 0.342 0.513 0.444 0.667 0.500 0.750 0.571 0.857 0.083 0.085 0.111
#>  CON1  CON2  CON3  LOC1  LOC2 
#> 0.667 0.684 0.889 0.750 0.769
```

When the available evidence supports different eligibility estimates for
the unknown categories, pass them as a named vector. Each value is the
probability that a case in that category is eligible:

``` r
e_by_class <- c(UH = 0.4, UR = 0.7, UO = 0.2)
outcomerate(freq, e = e_by_class, rate = c("RR3", "REF2", "CON2"))
#>   RR3  REF2  CON2
#> 0.388 0.097 0.777
```

A category may be omitted from a non-scalar `e` only when its aggregate
count is zero (after weighting, when weights are supplied). A length-one
value—including the result of `eligibility_rate()`—keeps the original
behavior and applies to every unknown category.

Dispositions do not always come in a tabulated format. Survey analysts
often work with microdata directly, where each row represents a sampled
case. The `outcomerate` package allows you to obtain rates using such a
format as well:

``` r
# define a vector of dispositions
x <- c("I", "P", "I", "UO", "R", "I", "NC", "I", "O", "P", "UH", "UR")

# calculate desired rates
outcomerate(x, rate = c("RR2", "CON1"))
#>  RR2 CON1 
#> 0.50 0.67

# obtain a weighted rate using illustrative base weights
w <- c(rep(1.3, 6), rep(2.5, 6))
outcomerate(x, weight = w, rate = c("RR2", "CON1"))
#>  RR2w CON1w 
#>  0.45  0.61
```

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
