#' outcomerate Formula Matrix (Internal Data)
#'
#' The `fmat` object is the internal dataset used by the `outcomerate` package.
#' It holds all definitions for the outcome rates. With the exception of
#' location rates, these are taken from the AAPOR Standard Definitions (2016).
#'
#' The data is a 3-dimensional binary array consisting of:
#'   * outcome: codes {I, P, R, NC, O, UH, UO, eUH, eUO, NE}
#'   * rate: the shorthand name for the rate (e.g. RR1)
#'   * side: numerator (NUM) and denominator (DEN)
#'
#' Given these three dimensions, each outcome rate can be defined as a rational
#' number (i.e. a fraction) consisting of a summation of frequencies of
#' outcome codes (where the matrix entries are nonzero).
#'
#' The input parameters given by the user are {I, P, R, NC, O, UH, UO} and
#' the parameter 'e'. The parameter e is multiplied by {UH, UO} internally so
#' as to produce {eUH, eUO}.
#'
#' The reason for this implementation is:
#'
#' a) It conforms to a DRY (don't repeat yourself) philosophy by
#'  holding all definitions in one place. These definitions can be used as
#'  upstream inputs to functions/test suites requiring them.
#'
#' b) It makes it easier to use intermediate steps in the formula calculations.
#'  For instance, it may be of use to a researchers to want to obtain the
#'  numerator/denominators of calculations, instead of only the output.
#'
#'  c) it makes it easy to compare the output
#'
#'  d) It is easier to maintain
#'
#'
#' @name fmat
#' @docType data
#' @references
#'   \url{https://www.aapor.org/Standards-Ethics/Standard-Definitions-(1).aspx}
#' @keywords data
#' @examples
#' fmat <- outcomerate:::fmat
#'
#' # Print the dimensions
#' dimnames(fmat)
#'
#' # Say we want to know the definition of Response Rate 2, RR2. We see
#' # below that the numerator (NUM) column is defined by the entries with a 1,
#' # or (I + P). Likewise, the denominator (DEN) is defined as
#' # (I + P + R + NC + O + UH + UO)
#' fmat[, "RR2", ]
#'
#'
#' # To use linear algebra, we define a zero-one numerator matrix 'N'
#' # and a zero-one denominator matrix 'D'. Our count of disposition codes
#' # is given here manually as 'x' (in the same order as N and D).
#' N = fmat[ , , 1]
#' D = fmat[ , , 2]
#' x <- c(I = 5, P = 2, R = 1, NC = 7, O = 3,
#'       UH = 4, UO = 8,  NE = 1, eUH = 3, eUO = 6)
#'
#' # Return all rates
#' (x %*% N) / (x %*% D)
#'
#'
#' # The same thing can be achieved with the apply family of functions
#' numden <- apply(x * fmat, 2:3, sum)
#' numden[, 1] / numden[, 2]
NULL


#' middleearth Dataset
#'
#' `middlearth` is a toy dataset consisting of 1691 fake survey interviews
#' conducted in J.R.R. Tolkien's fictional world of Middle Earth.
#'
#' Variables contained in the data:
#'   * __code:__ one of the outcome codes {I, P, R, NC, O, UH, UO, UH, UO, NE}
#'   * __outcome:__ A human-interpretable label for the `code` variable
#'   * __researcher__: An identifier for the researcher conducting the interview
#'   * __region__: The region of the respondent (one of five)
#'   * __Q1__: A hypothetical binary research question posed to respondents
#'   * __Q2__: A hypothetical continuous scale question posed to respondents
#'   * __day__: The day the interview took place (1 being the first day of
#'   fieldwork)
#'   * __race__: The race of the respondent in middle earth (Dwarf, Elf, Hobbit,
#'   Man, or Wizard)
#'   * __svywt__: The survey weight (inverse probability of selection)
#'
#' @name middleearth
#' @docType data
#' @keywords data
NULL
