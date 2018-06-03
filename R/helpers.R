#' Default value for NULL
#'
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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
  names(which(apply(fmat[c("eUH", "eUO"), ,], 2, any)))
}
