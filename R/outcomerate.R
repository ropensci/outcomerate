#' AAPOR Survey Outcome Rates
#'
#' Provides standardized outcome rates for surveys, primarily as defined by the
#' [American Association for Public Opinion Research
#' (AAPOR)](https://aapor.org/). Details can be found in the Standard
#' Definitions manual \insertCite{aapor}{outcomerate}.
#'
#' Survey and public opinion research often categorizes interview attempts for
#' a survey according to a set of outcome codes as follows:
#'
#' * I  = Complete interview
#' * P  = Partial interview
#' * R  = Refusal and break-off
#' * NC = Non-contact
#' * O  = Other eligible non-interview (2.30, 2.90)
#' * UH = Unknown if household/occupied housing unit (3.10)
#' * UR = Unknown if sampled unit is eligible/housing unit contains an eligible
#'   respondent (3.20)
#' * UO = Unknown, other (3.90)
#' * NE = Not eligible (4.0)
#'
#' `UR` is the 10th-edition aggregate symbol for 3.20, which the 9th edition
#' included under `UO`. Legacy `UO` inputs remain supported. With a scalar `e`,
#' moving a 3.20 case from `UO` to `UR` does not alter a rate. With
#' category-specific estimates it can, so new 3.20 cases should be coded `UR`
#' for standards conformance.
#'
#' These high-level classes are used to calculate outcome rates that
#' provide some measure of quality over the fieldwork. These outcome rates
#' are defined here as follows:
#'
#' The formulas below show the traditional scalar form `e(UH + UR + UO)`. If
#' `e` is supplied by category, that term is evaluated as
#' `e["UH"] * UH + e["UR"] * UR + e["UO"] * UO`. Each value is the
#' conditional probability that a case in that unknown category is ultimately
#' eligible for the survey, following the companion guidance in
#' \insertCite{aapor_e_2025}{outcomerate}. Calculate `e` separately for each
#' frame. One vector applies to the cases in one call; combining frames requires
#' a scientifically justified aggregation. Other design components, modes, or
#' phases may also require separate estimates when their mechanisms differ.
#' Document the scientific basis for every estimate.
#'
#' __AAPOR Response Rate__
#'
#' The proportion of sampled cases that yield a complete or partial interview,
#' depending on the selected definition.
#'
#' * RR1 = I / ((I + P) + (R + NC + O) + (UH + UR + UO))
#' * RR2 = (I + P) / ((I + P) + (R + NC + O) + (UH + UR + UO))
#' * RR3 = I / ((I + P) + (R + NC + O) + e(UH + UR + UO))
#' * RR4 = (I + P) / ((I + P) + (R + NC + O) + e(UH + UR + UO))
#' * RR5 = I / ((I + P) + (R + NC + O))
#' * RR6 = (I + P) / ((I + P) + (R + NC + O))
#'
#' RR5 and RR6 are appropriate only when no unknown cases are eligible or no
#' cases have unknown eligibility.
#'
#' __AAPOR Cooperation Rates__
#'
#' The proportion of all interviewed cases among eligible units ever contacted.
#' These printed formulas are AAPOR's household-level rates.
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
#' * REF1 = R / ((I + P) + (R + NC + O) + (UH + UR + UO))
#' * REF2 = R / ((I + P) + (R + NC + O) + e(UH + UR + UO))
#' * REF3 = R / ((I + P) + (R + NC + O))
#'
#' As with RR5 and RR6, excluding unknown cases from REF3 must be justified by
#' the study's actual eligibility situation.
#'
#' __AAPOR Contact Rates__
#'
#' The proportion of cases in which a responsible member of the housing unit is
#' reached. These printed formulas are AAPOR's household-level rates.
#'
#' * CON1 = ((I + P) + (R + O)) / ((I + P) + (R + NC + O) +
#'   (UH + UR + UO))
#' * CON2 = ((I + P) + (R + O)) / ((I + P) + (R + NC + O) +
#'   e(UH + UR + UO))
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
#' * LOC1 = ((I + P) + (R + O + NC)) / ((I + P) + (R + NC + O) +
#'   (UH + UR + UO))
#' * LOC2 = ((I + P) + (R + O + NC)) / ((I + P) + (R + NC + O) +
#'   e(UH + UR + UO))
#'
#' @references \insertAllCited
#'
#' @param x a character vector of disposition outcomes (I, P, R, NC, O, UH, UR,
#'   UO, or NE). Alternatively, a named vector/table of (weighted) disposition
#'   counts.
#' @param e a numeric eligibility estimate in `[0, 1]`. A length-one value is
#'   applied to all unknown dispositions. Alternatively, use a named vector
#'   such as `c(UH = 0.4, UR = 0.7, UO = 0.2)` for category-specific estimates.
#'   A non-scalar vector must contain one uniquely named value for every
#'   unknown category with a positive aggregate count (weighted when `weight`
#'   is supplied); categories with a zero count may be omitted.
#'   [eligibility_rate()] provides a default scalar estimate. If an
#'   `e`-dependent rate is explicitly requested when every unknown category has
#'   count zero, `e` may be omitted.
#' @param rate an optional character vector specifying the rates to be
#'   calculated. If `NULL` (the default), all rates available for the supplied
#'   value of `e` are returned.
#' @param weight an optional numeric vector that specifies the weight of each
#'   element in 'x' if x is a character vector or factor. For AAPOR weighted
#'   rates, use base weights (inverse selection probabilities); two-phase
#'   designs should also account for subsampling. If none is provided (the
#'   default), an unweighted estimate is returned. Individual zero weights are
#'   permitted, as required for phase-2-eligible cases that are not subsampled.
#'   Weights cannot be supplied with an already-aggregated named vector or
#'   table.
#' @param return_nd a logical to switch to having the function return the
#'   numerator and denominator instead of the rate. Defaults to FALSE.
#' @return If `return_nd = FALSE`, a named numeric vector containing the
#'   requested outcome rates. If `return_nd = TRUE`, a numeric matrix with one
#'   row per requested rate and columns `NUM` and `DEN` containing its numerator
#'   and denominator. Names for weighted rates have a `w` suffix.
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
#' # O  = Other eligible non-interview (2.30, 2.90)
#' # UH = Unknown if household/occupied housing unit (3.10)
#' # UR = Unknown if sampled unit is eligible/housing unit contains an eligible
#' #      respondent (3.20)
#' # UO = Unknown, other (3.90)
#' # NE = Not eligible (4.0)
#' x <- c("I", "P", "I", "NC", "UH", "I", "R", "NE",
#'       "UR", "UO", "I", "O", "P", "I")
#'
#' # calculate all rates
#' elr <- eligibility_rate(x)
#' outcomerate(x, e = elr)
#'
#' # use separate eligibility estimates for each unknown category
#' e_by_class <- c(UH = 0.4, UR = 0.7, UO = 0.2)
#' outcomerate(x, e = e_by_class, rate = c("RR3", "REF2", "CON2"))
#'
#' # return only one rate
#' outcomerate(x, rate = "COOP1")
#'
#' # calculate weighted rates using illustrative base weights
#' w <- seq(0.5, 1.8, length.out = length(x))
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

  outcomerate_from_counts(
    stats::setNames(as.numeric(freq), names(freq)),
    e = e,
    rate = rate,
    return_nd = return_nd,
    weighted = !is.null(weight)
  )
}

#' @noRd
#' @export
outcomerate.table <- function(x, e = NULL, rate = NULL, weight = NULL,
                              return_nd = FALSE) {

  assert_unweighted_counts(weight)

  # convert table to a labelled numeric vector
  freq <- stats::setNames(as.numeric(x), names(x))
  outcomerate_from_counts(
    freq,
    e = e,
    rate = rate,
    return_nd = return_nd
  )
}

#' @noRd
#' @export
outcomerate.numeric <- function(x, e = NULL, rate = NULL, weight = NULL,
                                return_nd = FALSE) {

  assert_unweighted_counts(weight)
  outcomerate_from_counts(x, e = e, rate = rate, return_nd = return_nd)
}

#' Calculate outcome rates from validated aggregate counts
#'
#' @noRd
outcomerate_from_counts <- function(x, e = NULL, rate = NULL,
                                    return_nd = FALSE, weighted = FALSE) {

  # default to return as many rates as possible
  rate <- rate %||% default_rates(e)

  # assert expectations that do not depend on completed disposition counts
  assert_rate(rate)
  assert_freq(x)

  # ensure vector is complete and ordered
  levs <- c("I", "P", "R", "NC", "O", "UH", "UR", "UO", "NE")
  x[setdiff(levs, names(x))] <- 0
  x <- x[levs]

  # validate and expand either a global or category-specific eligibility rate
  assert_e(e, rate, x)
  e_values <- normalize_e(e, rate)

  # estimate eligible unknowns
  unknowns <- unknown_dispositions()
  x[paste0("e", unknowns)] <- e_values * x[unknowns]

  # assert that order of outcomes match
  stopifnot(all(names(x) == dimnames(fmat)[[1]]))

  # calculate numerator and denominator
  m <- x * fmat[c(levs, "eUH", "eUR", "eUO"), rate, , drop = FALSE]
  numden <- colSums(m, dims = 1, na.rm = TRUE)

  # if weighted estimate, rename
  if (weighted) {
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
