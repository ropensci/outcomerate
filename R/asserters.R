#' Assert validity of parameter 'e'
#'
#' @noRd
assert_e <- function(e, rate, x) {

  # determine if given rates depend on e
  if (!any(req_e() %in% rate)) {
    return(invisible(TRUE))
  }

  unknowns <- unknown_dispositions()
  present <- unknowns[x[unknowns] > 0]

  # No estimate is needed when every unknown category contributes zero.
  if (is.null(e) && length(present) == 0) {
    return(invisible(TRUE))
  }
  if (is.null(e)) {
    needed_rates <- intersect(rate, req_e())
    stop("Rates {", paste0(needed_rates, collapse = ", "),
         "} require the parameter 'e' to be defined. If you have NE ",
         "values in 'x', try running eligibility_rate(x) to estimate it.")
  }

  assert_e_values(e)
  if (length(e) > 1) {
    assert_e_names(e, unknowns, present)
  }

  invisible(TRUE)
}

#' Assert validity of eligibility-estimate values
#'
#' @noRd
assert_e_values <- function(e) {
  if (!is.numeric(e)) {
    stop("The parameter e must be numeric")
  }
  if (length(e) < 1) {
    stop("The parameter e must contain at least one value")
  }
  if (!all(is.finite(e))) {
    stop("The parameter e must contain only finite, non-missing values")
  }
  if (any(e < 0 | e > 1)) {
    stop("The parameter e must be on the interval [0, 1]")
  }

  invisible(TRUE)
}

#' Assert validity of category-specific eligibility-estimate names
#'
#' @noRd
assert_e_names <- function(e, unknowns, present) {
  if (is.null(names(e)) || anyNA(names(e)) || any(names(e) == "")) {
    stop("A non-scalar e must be named using UH, UR, and UO")
  }
  if (anyDuplicated(names(e))) {
    stop("Names in a non-scalar e must be unique")
  }

  invalid <- setdiff(names(e), unknowns)
  if (length(invalid) > 0) {
    stop("Names in a non-scalar e must be drawn from {UH, UR, UO}: ",
         paste0(invalid, collapse = ", "))
  }

  missing <- setdiff(present, names(e))
  if (length(missing) > 0) {
    stop("The parameter e is missing estimates for dispositions present ",
         "in x: ", paste0(missing, collapse = ", "))
  }

  invisible(TRUE)
}

#' Assert validity of vector of outcomes
#'
#' @noRd
assert_disposition <- function(x) {

  if (anyNA(x)) {
    stop("The input 'x' contains NA values. Consider converting them to \n",
         "NE (not eligible) or UH / UR / UO (unknown eligibility)")
  }

  invisible(TRUE)
}

#' Assert validity of frequency vector
#'
#' @noRd
assert_freq <- function(x) {

  codes <- c("I", "P", "NC", "R", "O", "UH", "UR", "UO", "NE")
  if (is.null(names(x))) {
    stop("The input 'x' should be a named vector")
  }
  if (anyNA(names(x)) || any(names(x) == "")) {
    stop("All elements in 'x' should be named")
  }
  if (anyDuplicated(names(x))) {
    stop("Disposition names in 'x' must be unique")
  }
  if (anyNA(x)) {
    stop("Disposition counts must not contain NA values")
  }
  if (!all(is.finite(x))) {
    stop("Disposition counts must be finite")
  }
  if (any(x < 0)) {
    stop("Disposition counts must be non-negative")
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

#' Reject weights for already-aggregated inputs
#'
#' @noRd
assert_unweighted_counts <- function(weight) {
  if (!is.null(weight)) {
    stop(
      "'weight' can only be supplied when 'x' contains individual ",
      "dispositions, not aggregate counts"
    )
  }

  invisible(TRUE)
}

#' Assert validity of weight vector
#'
#' @noRd
assert_weight <- function(weight, x) {

  # conditions that must be met if weight is non-null
  if (!is.null(weight)) {
    if (!is.numeric(weight)) {
      stop("weights must be numeric")
    }
    if (length(weight) != length(x)) {
      stop("weight must be same length as 'x'")
    }
    if (anyNA(weight)) {
      stop("weights must not contain NA values")
    }
    if (!all(is.finite(weight))) {
      stop("weights must be finite")
    }
    if (any(weight < 0)) {
      stop("weights must be non-negative")
    }
    if (all(weight == 0)) {
      stop("weights must not all be zero")
    }
  }

  invisible(TRUE)
}

#' Assert validity of rates
#'
#' @noRd
assert_rate <- function(rate) {

  if (!is.null(rate)) {
    # throw error if any inputs are not in the set of
    # expected rates
    unrecognized <- setdiff(rate, dimnames(fmat)$rate)
    if (length(unrecognized) > 0) {
      stop("The following rates are not recognized: ",
           paste0(unrecognized, collapse = ", "))
    }

  }

  invisible(TRUE)
}
