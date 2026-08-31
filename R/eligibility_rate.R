#' Survey Eligibility Rate
#'
#' Provides an estimate for the proportion of cases of unknown eligibility
#' that are eligible, as described by \insertCite{vdk}{outcomerate}. The
#' rate is typically (but not necessarily) calculated on the screener data
#' or other sources depending on the type of survey, and approaches to
#' calculating 'e' may therefore differ from one survey to the next.
#'
#' The present proportional-allocation implementation follows the default used
#' in the Excel-based [AAPOR Outcome Rate Calculator (Version 5.1, April
#' 2023)](https://aapor.org/wp-content/uploads/2023/06/Response-Rate-Calculator-5-1_04142023.xlsx),
#' on the basis of known ineligibles being coded as "NE". It is one accepted
#' estimator of `e`; researchers should use better design-specific information
#' when available. This function returns one scalar estimate. Separate
#' estimates can be supplied directly to [outcomerate()] as a named vector such
#' as `c(UH = 0.4, UR = 0.7, UO = 0.2)`; they cannot be inferred from the
#' package's aggregate `NE` count alone. See
#' \insertCite{aapor_e_2025}{outcomerate} for category-specific estimation
#' guidance.
#'
#'
#' The eligibility rate (ELR) is defined as
#'
#' * ELR = (I + P + R + NC + O) / (I + P + R + NC + O + NE)
#'
#' @references \insertRef{aapor}{outcomerate} \insertAllCited
#'
#' @param x a character vector of disposition outcomes (I, P, R, NC, O, UH, UR,
#'   UO, or NE). Alternatively, a named vector/table of (weighted) disposition
#'   counts.
#' @param weight an optional numeric vector that specifies the weight of each
#'   element in 'x' if x is a character vector. For probability samples, these
#'   will normally be base weights (inverse selection probabilities). If none
#'   is provided (the default), an unweighted estimate is returned.
#' @importFrom Rdpack reprompt
#' @export
#' @seealso [outcomerate]
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
#' x <- c("I", "P", "I", "NE", "NC", "UH", "I", "R", "UR", "UO", "I", "O",
#'        "P", "I")
#'
#' # estimate the eligibility rate
#' eligibility_rate(x)
#'
#' # calculate a weighted rate using illustrative base weights
#' w <- seq(0.5, 1.8, length.out = length(x))
#' eligibility_rate(x, weight = w)
#'
#' # alternatively, provide input as counts
#' freq <- c(I = 6, P = 2, NC = 3, NE = 1)
#' eligibility_rate(freq)
#'
eligibility_rate <- function(x, weight = NULL) {
  UseMethod("eligibility_rate", x)
}

#' @noRd
#' @export
eligibility_rate.character <- function(x, weight = NULL) {

  # assert expectations
  assert_disposition(x)
  assert_weight(weight, x)

  # produce weighted frequencies
  weight <- weight %||% rep(1, length(x))
  freq   <- stats::xtabs(weight ~ x)

  eligibility_rate(freq)
}


#' @noRd
#' @export
eligibility_rate.table <- function(x, ...) {

  # convert table to a labelled numeric vector
  freq <- stats::setNames(as.numeric(x), names(x))
  eligibility_rate(freq)
}

#' @noRd
#' @export
eligibility_rate.numeric <- function(x, ...) {

  # assert expectations
  assert_freq(x)
  if (!"NE" %in% names(x)) {
    warning("No 'NE' values found in 'x'. This implies 100% eligibility.")
  }

  # ensure vector is complete and ordered
  levs <- c("NE", "I", "P", "R", "NC", "O")
  x[setdiff(levs, names(x))] <- 0
  num <- x[c("I", "P", "R", "NC", "O")]
  den <- x[c("NE", "I", "P", "R", "NC", "O")]


  # calculate rate
  elr <- c(ELR = sum(num) / sum(den))

  # return outputs
  elr
}

#' @noRd
#' @export
eligibility_rate.factor <- eligibility_rate.character
