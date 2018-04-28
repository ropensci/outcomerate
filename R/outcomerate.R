#' AAPOR Survey Outcome Rates
#'
#' Provides standardized outcome rates for surveys, primarily as defined by the
#' American Association for Public Opinion Research (AAPOR). Details can be
#' found in the Standard Definitions manual \insertCite{aapor}{outcomerate}.
#'
#'
#' The outcome rates are defined as follows:
#'
#' AAPOR Response Rates:
#'
#' \itemize{ \item{RR1}{ = I / [(I + P) + (R + NC + O) + (UH + UO)]} \item{RR2}{
#' = (I + P) / [(I + P) + (R + NC + O) + (UH + UO)]} \item{RR3}{ = I / [(I + P)
#' + (R + NC + O) + e(UH + UO)]} \item{RR4}{ = (I + P) / [(I + P) + (R + NC + O)
#' + e(UH + UO)]} \item{RR5}{ = I / [(I + P) + (R + NC + O)]} \item{RR6}{ = (I +
#' P) / [(I + P) + (R + NC + O)]} }
#'
#' AAPOR Cooperation Rates:
#'
#' \itemize{ \item{COOP1}{ = I / [(I + P) + R + O]} \item{COOP2}{ = (I + P) /
#' [(I + P) + R + O]} \item{COOP3}{ = I / [(I + P) + R]} \item{COOP4}{ = (I + P)
#' / [(I + P) + R]} }
#'
#' AAPOR Refusal Rates:
#'
#' \itemize{ \item{REF1}{ = R / [(I + P) + (R + NC + O) + (UH + UO)]}
#' \item{REF2}{ = R / [(I + P) + (R + NC + O) + e(UH + UO)]} \item{REF3}{ = R /
#' [(I + P) + (R + NC + O)]} }
#'
#' AAPOR Contact Rates:
#'
#' \itemize{ \item{CON1}{ = [(I + P) + (R + O)] / [(I + P) + (R + NC + O) + (UH
#' + UO)]} \item{CON2}{ = [(I + P) + (R + O)] / [(I + P) + (R + NC + O) + e(UH +
#' UO)]} \item{CON3}{ = [(I + P) + (R + O)] / [(I + P) + (R + NC + O)]} }
#'
#' Location Rate:
#'
#' The location rate is not defined in AAPOR's standard, but can be found in
#' \insertCite{vdk}{outcomerate}.
#'
#' \itemize{ \item{LOC1}{ = [(I + P) + (R + O + NC)] / [(I + P) + (R + NC + O) +
#' (UH + UO)]} \item{LOC2}{ = [(I + P) + (R + O + NC)] / [(I + P) + (R + NC + O)
#' + e(UH + UO)]} }
#'
#' @references \insertAllCited \insertRef{aapor}{outcomerate}
#'
#' @param x a character vector of disposition outcomes (I, P, R, NC, O, UH, or
#'   UO). Alternatively, a named vector/table of (weighted) disposition counts.
#' @param e a scalar number that specifies the estimated proportion of unknown
#'   cases (U) which are eligible.
#' @param rate an optional character vector specifying the rates to be
#'   calculated. If set to NA (the default), all rates are returned.
#' @param weight an optional numeric vector that specifies the weight of each
#'   element in 'x' if x is a character vector. If none is provided (the
#'   default), an unweighted estimate is returned.
#' @param return_nd a logical to switch to having the function return the
#'   numerator and denominator instead of the rate. Defaults to FALSE.
#' @importFrom Rdpack reprompt
#' @export
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
#' x <- c("I", "P", "I", "NC", "UH", "I", "R", "UO", "I", "O", "P", "I")
#'
#' # calculate all rates, assume 80% of unknown cases are elligble
#' outcomerate(x, e = 0.8)
#'
#' # return only one rate
#' outcomerate(x, rate = "COOP1")
#'
#' # calculate weighted rates
#' w <- runif(12, 0, 5)
#' outcomerate(x, e = 0.8, weight = w)
#'
#' # alternatively, provide input as counts
#' freq <- c(I = 6, P = 2, NC = 3, R = 1)
#' outcomerate(freq, e = 0.8)
outcomerate <- function(x, e = NA, rate = NA, weight = NA,
                        return_nd = FALSE) {
  UseMethod("outcomerate", x)
}

#' @noRd
#' @export
outcomerate.character <- function(x, e = NA, rate = NA, weight = NA,
                                  return_nd = FALSE) {

  # assert expectations
  assert_disposition(x)
  assert_weight(weight)
  if (!length(weight) %in% c(1, length(x))) {
    stop("weight must be same length as 'x' (or scalar)")
  }

  # count cases, weight if applicable
  if (length(weight) == 1 && is.na(weight)) {
    tab <- table(x)
    freq <- as.numeric(tab)
    names(freq) <- names(tab)
  } else if (length(weight) == 1) {
    tab <- table(x) * weight
    freq <- as.numeric(tab)
    names(freq) <- names(tab)
  } else {
    freq <- vapply(split(weight, x), sum, numeric(1))
  }

  outcomerate(freq, e = e, rate = rate, return_nd = return_nd)
}

#' @noRd
#' @export
outcomerate.numeric <- function(x, e = NA, rate = NA, weight = NA,
                                return_nd = FALSE) {

  # default to return all rates
  if (identical(rate, NA)) rate <- dimnames(fmat)$rate

  # assert expectations
  assert_e(e, rate)
  assert_freq(x)

  # ensure vector is complete and ordered
  levs <- c("I", "P", "R", "NC", "O", "UH", "UO")
  x[setdiff(levs, names(x))] <- 0
  x <- x[levs]

  # estimate elligible unknowns
  x["eUH"] <- e * x["UH"]
  x["eUO"] <- e * x["UO"]

  # calculate numerator and denominator
  m <- x * fmat[, rate, , drop = FALSE]
  numden <- apply(m, 2:3, sum, na.rm = TRUE)

  # calculate rates
  rates <- apply(numden, 1, function(x) x[1] / x[2])

  # return outputs
  if (return_nd) numden else rates
}

#' @noRd
#' @export
outcomerate.table <- function(x, e = NA, rate = NA, weight = NA,
                              return_nd = FALSE) {
  freq <- as.numeric(x)
  names(freq) <- names(x)
  outcomerate(freq, e = e, rate = rate, return_nd = return_nd)
}

#' @noRd
#' @export
outcomerate.factor <- function(x, e = NA, rate = NA, weight = NA,
                              return_nd = FALSE) {
  outcomerate(as.character(x), e = e, rate = rate,
              weight = weight,
              return_nd = return_nd)
}

#' Middle Earth Survey Dataset (Fake)
#'
#' @name middleearth
#' @docType data
#' @keywords data
NULL
