#' Elements to Compare to Before Fit
#'
#' [bdlim1()] return a list. These are the elements that we need to check to
#' make sure our edits didn't change the results.
#'
#' @return vector of character
#' @keywords internal
#' @noRd
elements_to_test <- function() {
  out <- c(
    "w", "regcoef", "sigma", "loglik", "names_groups", "n_times",
    "dlfun", "ce", "WAIC", "n", "nits", "nburn", "nthin", "REmodel",
    "family", "call"
  )

  return(out)
}
