
[![Coverage
status](https://codecov.io/gh/ropensci/outcomerate/branch/master/graph/badge.svg)](https://codecov.io/github/ropensci/outcomerate?branch=master)
[![Travis build
status](https://travis-ci.org/ropensci/outcomerate.svg?branch=master)](https://travis-ci.org/ropensci/outcomerate)
[![Ropensci
status](https://badges.ropensci.org/213_status.svg)](https://github.com/ropensci/onboarding/issues/213)

# outcomerate

`outcomerate` is a lightweight R package that implements the standard
outcome rates for surveys, as defined in the [Standard
Definitions](https://www.aapor.org/Standards-Ethics/Standard-Definitions-\(1\).aspx)
of the American Association of Public Opinion Research (AAPOR).

Although the mathematical formulas are straightforward, it can get
tedious and repetitive calculating all the rates by hand, especially for
sub-groups of your study. The formulas are similar to one another and so
it is also dangerously easy to make a clerical mistake. The
`outcomerate` package simplifies the analytically workflow by defining
all formulas as a collection of functions.

## Installation

The latest development version is available via github:

``` r
install.packages("devtools")
devtools::install_github("ropensci/outcomerate")
```

## Example

Let’s say you try to survey 12 people. After finishing the fieldwork,
you tabulate all your attempts into a table of disposition outcomes:

| code | disposition           | n |
| :--- | :-------------------- | -: |
| I    | Complete interview    | 4 |
| P    | Partial interview     | 2 |
| R    | Refusal and break-off | 1 |
| NC   | Non-contact           | 1 |
| O    | Other                 | 1 |
| UH   | Unknown if household  | 1 |
| NE   | Known ineligible      | 1 |
| UO   | Unknown, other        | 1 |

Using this table, you may wish to report some of the common survey
outcome rates, such as:

  - **Response Rate:** The proportion of your sample that results in an
    interview.
  - **Cooperation Rate:** The proportion of people contacted who
    participate in your survey.
  - **Refusal Rate:** The proportion of your sample that refused to
    participate.
  - **Contact Rate:** The proportion of sampled cases where you manage
    to reach the respondent.
  - **Location Rate:** The proportion of cases (say, in an establishment
    survey) that you manage to locate.

Most of these rates come under a number of variants, having definitions
that are standardized by AAPOR. The `outcomerate` function lets your
calculate these rates seamlessly:

``` r
# load package
library(outcomerate)

# set counts per disposition code (needs to be a named vector)
freq <- c(I = 4, P = 2, R = 1, NC = 1, O = 1, UH = 1, UO = 1, NE = 1)

# calculate rates, assuming 90% of unknown cases are elligble
outcomerate(freq, e = eligibility_rate(freq))
#>   RR1   RR2   RR3   RR4   RR5   RR6 COOP1 COOP2 COOP3 COOP4  REF1  REF2 
#> 0.364 0.545 0.370 0.556 0.444 0.667 0.500 0.750 0.571 0.857 0.091 0.093 
#>  REF3  CON1  CON2  CON3  LOC1  LOC2 
#> 0.111 0.727 0.741 0.889 0.818 0.833
```

Dispositions do not always come in a tabulated format. Survey analysts
often work with microdata directly, where each row represents an
interview. The `outcomerate` package allows you to obtain rates using
such a format as well:

``` r
# define a vector of dispositions
x <- c("I", "P", "I", "UO", "R", "I", "NC", "I", "O", "P", "UH")

# calculate desired rates
outcomerate(x, rate = c("RR2", "CON1"))
#>  RR2 CON1 
#> 0.55 0.73

# obtain a weighted rate
w <- c(rep(1.3, 6), rep(2.5, 5))
outcomerate(x, weight = w, rate = c("RR2", "CON1"))
#>  RR2w CON1w 
#>  0.50  0.69
```

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
