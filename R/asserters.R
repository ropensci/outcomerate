#' Assert validity of parameter 'e'
#'
#' @noRd
assert_e <- function(e, rate) {

  # determine if given rates depend on e
  needs_e <- any(req_e() %in% rate)
  if (needs_e) {
    if (length(e) != 1) {
      stop("The parameter e must be a scalar value")
    }
    if (!is.numeric(e)) {
      stop("The parameter e must be numeric")
    }
    if (e < 0 | e > 1) {
      stop("The parameter e must be on the interval [0, 1]")
    }
  }
  invisible(TRUE)
}

#' Assert validity of vector of outcomes
#'
#' @noRd
assert_disposition <- function(x) {

  if (any(is.na(x))) {
    stop("The input 'x' contains NA values. Consider converting them to \n",
         "NE (known inelligibles) or UO / UH (unknown elligibility)")
  }

  invisible(TRUE)
}

#' Assert validity of frequency vector
#'
#' @noRd
assert_freq <- function(x) {

  codes <- c("I", "P", "NC", "R", "O", "UH", "UO", "NE")
  if (is.null(names(x))) {
    stop("The input 'x' should be a named vector")
  }
  if (any(names(x) == "")) {
    stop("All elements in 'x' should be named")
  }
  if (!all(names(x) %in% codes)) {
    unk <- setdiff(names(x), codes)
    msg <- paste0(unk, collapse = ", ")
    stop("Certain names in 'x' are not valid: ", msg,
         "\nEnsure they are in the set {",
         paste0(codes, collapse = ", "), "}")
  }

  invisible(TRUE)
}

#' Assert validity of weight vector
#'
#' @noRd
assert_weight <- function(weight, x) {

  # conditions that must be met if weight is non-null
  if (!is.null(weight)) {
    if (length(weight) != length(x)) {
      stop("weight must be same length as 'x'")
    }
    if (any(is.na(weight))) {
      stop("weights must not contain NA values")
    }
    if (any(weight == 0)) {
      warning("weights contain contain zeros")
    }
  }

  invisible(TRUE)
}

#' Assert validity of rates
#'
#' @noRd
assert_rate <- function(rate, e) {

  if (!is.null(rate)) {
    # throw error if any inputs are not in the set of
    # expected rates
    unrecognized <- setdiff(rate, dimnames(fmat)$rate)
    if (length(unrecognized) > 0) {
      stop("The following rates are not recognized: ",
           paste0(unrecognized, collapse = ", "))
    }

    # list input rates requiring e
    emat <- fmat[c("eUH", "eUO"), rate, , drop = FALSE]
    req_e <- apply(emat, 2, sum) > 0

    # throw error if any of the requested rates require e,
    # and e is not provided
    if (any(req_e) && is.null(e)) {
      msg <- paste0(names(which(req_e)), collapse = ", ")
      stop("Rates {", msg, "} require the parameter 'e' to be ",
           "defined. If you have NE values in 'x', try running ",
           "eligibility_rate(x) to estimate it.")
    }
  }

  invisible(TRUE)
}
