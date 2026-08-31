#' Default value for NULL
#'
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Unknown-eligibility aggregate codes
#'
#' @noRd
unknown_dispositions <- function() {
  c("UH", "UR", "UO")
}

#' Expand scalar or category-specific eligibility estimates
#'
#' Returns one eligibility estimate for each unknown disposition. Validation is
#' handled by `assert_e()` before this helper is called.
#'
#' @noRd
normalize_e <- function(e, rate) {
  unknowns <- unknown_dispositions()
  values <- stats::setNames(rep(0, length(unknowns)), unknowns)

  # Ignore e when none of the requested rates uses an eligibility estimate.
  if (!any(req_e() %in% rate)) {
    return(values)
  }

  if (length(e) == 1) {
    values[] <- unname(e)
  } else {
    values[names(e)] <- unname(e)
  }

  values
}

#' Return default rates
#'
#' A function that returns default rates, depending on the parameter 'e'
#'
#' @noRd
default_rates <- function(e = NULL){
  all_rates <- dimnames(fmat)$rate
  if (is.null(e)) {
    setdiff(all_rates, req_e())
  } else {
    all_rates
  }
}


#' List outcome rates that depend on 'e'
#'
#' @noRd
req_e <- function(){
  names(which(apply(fmat[c("eUH", "eUR", "eUO"), ,], 2, any)))
}
