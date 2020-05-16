#' AAPOR Survey Outcome Rates
#'
#' Provides standardized outcome rates for surveys, primarily as defined by the
#' [American Association for Public Opinion Research
#' (AAPOR)](http://www.aapor.org/). Details can be found in the Standard
#' Definitions manual \insertCite{aapor}{outcomerate}.
#'
#' Survey and public opinion research often categorizes interview attempts of
#' of a survey according to a set of outcome codes as follows:
#'
#' * I  = Complete interview
#' * P  = Partial interview
#' * R  = Refusal and break-off
#' * NC = Non-contact
#' * O  = Other
#' * UH = Unknown if household/occupied housing unit
#' * UO = Unknown, other
#' * NE = Known ineligible
#'
#' These high-level classes are used to calculate outcome rates that
#' provide some measure of quality over the fieldwork. These outcome rates
#' are defined here as follows:
#'
#' __AAPOR Response Rate__
#'
#' The proportion of your intended sample that participate in the survey.
#'
#' * RR1 = I / ((I + P) + (R + NC + O) + (UH + UO))
#' * RR2 = (I + P) / ((I + P) + (R + NC + O) + (UH + UO))
#' * RR3 = I / ((I + P) + (R + NC + O) + e(UH + UO))
#' * RR4 = (I + P) / ((I + P) + (R + NC + O) + e(UH + UO))
#' * RR5 = I / ((I + P) + (R + NC + O))
#' * RR6 = (I + P) / ((I + P) + (R + NC + O))
#'
#' __AAPOR Cooperation Rates__
#'
#' The proportion of contacted respondents who participate in the survey.
#'
#' * COOP1 = I / ((I + P) + R + O)
#' * COOP2 = (I + P) / ((I + P) + R + O)
#' * COOP3 = I / ((I + P) + R)
#' * COOP4 = (I + P) / ((I + P) + R)
#'
#' __AAPOR Refusal Rates__
#'
#' The proportion of the sample that refuses to participate in the survey.
#'
#' * REF1 = R / ((I + P) + (R + NC + O) + (UH + UO))
#' * REF2 = R / ((I + P) + (R + NC + O) + e(UH + UO))
#' * REF3 = R / ((I + P) + (R + NC + O))
#'
#' __AAPOR Contact Rates__
#'
#' The proportion of the sample that is successfully contacted for
#'  an interview (whether they chose to participate or not).
#'
#' * CON1 = ((I + P) + (R + O)) / ((I + P) + (R + NC + O) + (UH+ UO))
#' * CON2 = ((I + P) + (R + O)) / ((I + P) + (R + NC + O) + e(UH + UO))
#' * CON3 = ((I + P) + (R + O)) / ((I + P) + (R + NC + O))
#'
#' __Location Rate__
#'
#' The proportion of cases that could be located for an interview.
#'
#' The location rate is not defined in AAPOR's Standards, but can be found in
#' \insertCite{vdk}{outcomerate}. Note: depending on how the
#' located cases are encoded, this may or may not be the correct formula.
#'
#' * LOC1 = ((I + P) + (R + O + NC)) / ((I + P) + (R + NC + O) + (UH + UO))
#' * LOC2 = ((I + P) + (R + O + NC)) / ((I + P) + (R + NC + O) + e(UH + UO))
#'
#' @references \insertAllCited \insertRef{aapor}{outcomerate}
#'
#' @param x a character vector of disposition outcomes (I, P, R, NC, O, UH, or
#'   UO). Alternatively, a named vector/table of (weighted) disposition counts.
#' @param e a scalar number that specifies the eligibility rate (the estimated
#'   proportion of unknown cases which are eligible). A default method
#'   of calculating 'e' is provided by [eligibility_rate()].
#' @param rate an optional character vector specifying the rates to be
#'   calculated. If set to NA (the default), all rates are returned.
#' @param weight an optional numeric vector that specifies the weight of each
#'   element in 'x' if x is a character vector or factor. If none is provided
#'   (the default), an unweighted estimate is returned.
#' @param return_nd a logical to switch to having the function return the
#'   numerator and denominator instead of the rate. Defaults to FALSE.
#' @importFrom Rdpack reprompt
#' @export
#' @md
#'
#' @examples
#' # load the outcomerate package
#' library(outcomerate)
#'
#' # Create a vector of survey dispositions
#' #
#' # I  = Complete interview
#' # P  = Partial interview
#' # R  = Refusal and break-off
#' # NC = Non-contact
#' # O  = Other
#' # UH = Unknown if household/occupied housing unit
#' # UO = Unknown, other
#' # NE = Known ineligible
#' x <- c("I", "P", "I", "NC", "UH", "I", "R", "NE",
#'       "UO", "I", "O", "P", "I")
#'
#' # calculate all rates
#' elr <- eligibility_rate(x)
#' outcomerate(x, e = elr)
#'
#' # return only one rate
#' outcomerate(x, rate = "COOP1")
#'
#' # calculate weighted rates
#' w <- runif(length(x), 0, 5)
#' outcomerate(x, e = elr, weight = w)
#'
#' # alternatively, provide input as counts
#' freq <- c(I = 6, P = 2, NC = 3, R = 1)
#' outcomerate(freq, e = elr)
outcomerate <- function(x, e = NULL, rate = NULL, weight = NULL,
                        return_nd = FALSE) {
  UseMethod("outcomerate", x)
}

#' @noRd
#' @export
outcomerate.character <- function(x, e = NULL, rate = NULL, weight = NULL,
                                  return_nd = FALSE) {

  # assert expectations
  assert_disposition(x)
  assert_weight(weight, x)

  # produce weighted frequencies
  w <- weight %||% rep(1, length(x))
  freq   <- stats::xtabs(w ~ x)

  # call table method
  outcomerate(freq, e = e, rate = rate, return_nd = return_nd, weight = weight)
}

#' @noRd
#' @export
outcomerate.table <- function(x, e = NULL, rate = NULL, weight = NULL,
                              return_nd = FALSE) {

  # convert table to a labelled numeric vector
  freq <- stats::setNames(as.numeric(x), names(x))
  outcomerate(freq, e = e, rate = rate, return_nd = return_nd, weight = weight)
}

#' @noRd
#' @export
outcomerate.numeric <- function(x, e = NULL, rate = NULL, weight = NULL,
                                return_nd = FALSE) {

  # default to return as many rates as possible
  rate <- rate %||% default_rates(e)

  # assert expectations
  assert_rate(rate, e)
  assert_e(e, rate)
  assert_freq(x)

  # ensure vector is complete and ordered
  levs <- c("I", "P", "R", "NC", "O", "UH", "UO", "NE")
  x[setdiff(levs, names(x))] <- 0
  x <- x[levs]

  # estimate elligible unknowns
  e <- e %||% 0
  x["eUH"] <- e * x["UH"]
  x["eUO"] <- e * x["UO"]

  # assert that order of outcomes match
  stopifnot(all(names(x) == dimnames(fmat)[[1]]))

  # calculate numerator and denominator
  m <- x * fmat[c(levs, "eUH", "eUO"), rate, , drop = FALSE]
  numden <- apply(m, 2:3, sum, na.rm = TRUE)

  # if weighted estimate, rename
  if (!is.null(weight)) {
    dimnames(numden)$rate <- paste0(dimnames(numden)$rate, "w")
  }

  # calculate rates (keep names)
  rates <- apply(numden, 1, function(x) x[1] / x[2])

  # return outputs
  if (return_nd) numden else rates
}


#' @noRd
#' @export
outcomerate.factor <- outcomerate.character

