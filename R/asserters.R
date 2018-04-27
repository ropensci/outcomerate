#' Assert validity of parameter 'e'
#'
#' @noRd
assert_e <- function(e, rate) {
  # determine if given rates depend on e
  req <- c("RR3", "RR4", "REF2", "CON2")
  needs_e <- any(req %in% rate)

  if (needs_e && is.na(e)) {
    problem_rate <- paste(intersect(rate, req), collapse = ", ")
    stop("The parameter e must be provided for ", problem_rate)
  }
  if (needs_e && !is.numeric(e)) {
    stop("The parameter e must be numeric")
  }
  if (needs_e && (e < 0 | e > 1)) {
    stop("The parameter e must be on the interval [0, 1]")
  }
  if (needs_e && length(e) != 1) {
    stop("The parameter e must be a scalar value")
  }
  invisible(TRUE)
}

#' Assert validity of vector of outcomes
#'
#' @noRd
assert_disposition <- function(x) {
  if (any(is.na(x))) {
    stop("The input 'x' contains NA values")
  }
  if (!all(x %in% c("I", "P", "NC", "R", "UH", "UO", "O"))) {
    stop("The input 'x' should only contain I, P, NC, R, UH, UO, and O")
  }
  invisible(TRUE)
}

#' Assert validity of frequency vector
#'
#' @noRd
assert_freq <- function(x) {
  if (!all(names(x) %in% c("I", "P", "NC", "R", "UH", "UO", "O"))) {
    stop("The input 'x' should be a named vector")
  }
  invisible(TRUE)
}

#' Assert validity of weight vector
#'
#' @noRd
assert_weight <- function(weight) {
  is_default <- identical(weight, NA)
  if (!is_default && sum(weight) == 0) {
    warning("The sum of weights is zero.")
  }
  invisible(TRUE)
}
