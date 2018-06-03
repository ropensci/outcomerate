#' Survey Eligibility Rate
#'
#' Provides an estimate for the proportion of cases of unknown eligibility
#' that are eligible, as described by \insertCite{vdk}{outcomerate}. The
#' rate is typically (but not necessarily) calculated on the screener data
#' or other sources depending on the type of survey, and approaches to
#' calculating 'e' may therefore differ from one survey to the next.
#'
#' The present implementation follows the default used in the Excel-based _AAPOR
#' Outcome Rate Calculator (Version 4.0, May, 2016)_ on the basis of
#' known ineligibles being coded as "NE".
#'
#'
#' The eligibility rate (ELR) is defined as
#'
#' * ELR = (I + P + R + NC + O) / (I + P + R + NC + O + NE)
#'
#' @references \insertRef{aapor}{outcomerate} \insertAllCited
#'
#' @param x a character vector of disposition outcomes (I, P, R, NC, O, UH, UO,
#'   U, or NE). Alternatively, a named vector/table of (weighted) disposition
#'   counts.
#' @param weight an optional numeric vector that specifies the weight of each
#'   element in 'x' if x is a character vector. If none is provided (the
#'   default), an unweighted estimate is returned.
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
#' # O  = Other
#' # UH = Unknown if household/occupied housing unit
#' # UO = Unknown, other
#' # NE = Not eligible
#' x <- c("I", "P", "I", "NE", "NC", "UH", "I", "R", "UO", "I", "O", "P", "I")
#'
#' # calculate all rates, assume 80% of unknown cases are elligble
#' eligibility_rate(x)
#'
#' # calculate weighted rates
#' w <- runif(13, 0, 5)
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
