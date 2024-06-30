#' Get Model Name Based on Parameter Constraints
#'
#' This function determines the model name based on whether the 'w' and 'b'
#' parameters are free or constrained.
#'
#' @param w_free A logical value. If TRUE, the 'w' parameter is free; if FALSE, it's constrained.
#' @param b_free A logical value. If TRUE, the 'b' parameter is free; if FALSE, it's constrained.
#'
#' @return A character string representing the model name:
#'   \itemize{
#'     \item "bw" if both 'w' and 'b' are free
#'     \item "b" if only 'b' is free
#'     \item "w" if only 'w' is free
#'     \item "n" if neither 'w' nor 'b' is free
#'   }
#'
#' @keywords internal
get_model_name <- function(w_free, b_free) {
  if (!is.logical(w_free) || !is.logical(b_free)) {
    stop("Both w_free and b_free must be logical values")
  }

  return(c("n", "b", "w", "bw")[1 + 2 * w_free + b_free])
}

#' Check MCMC Convergence
#'
#' Checks the convergence of MCMC draws using `rhat` and effective sample sizes (`ess_bulk` and `ess_tail`).
#'
#' @param object An object of class `posterior`, containing MCMC samples.
#' @examples
#' \dontrun{
#' options(bdlim_rhat_thresh = 1.05)
#' options(bdlim_ess_thresh = 500)
#'
#' # check_mcmc_convergence(summary(bdlim4_fit))
#' }
#'
#' @keywords internal
#' @references https://projecteuclid.org/journals/bayesian-analysis/volume-16/issue-2/Rank-Normalization-Folding-and-Localization--An-Improved-R%CB%86-for/10.1214/20-BA1221.full
check_mcmc_convergence <- function(object) {
  rhat_thesh <- getOption("bdlim_rhat_thresh", 1.01)
  ess_thesh <- getOption("bdlim_ess_thresh", 400)

  if (any(object$MCMC_check$rhat >= rhat_thesh)) {
    warning("Some parameters have rhat >= ", rhat_thesh, ". Increase `nits` and `nthin` to improve convergence.")
  }
  if (any(object$MCMC_check$ess_bulk < ess_thesh)) {
    warning("Some parameters have ess_bulk < ", ess_thesh, ". Increase `nits` and `nthin` to improve effective sample size.")
  }
  if (any(object$MCMC_check$ess_tail < ess_thesh)) {
    warning("Some parameters have ess_tail < ", ess_thesh, ". Increase `nits` and `nthin` to improve effective sample size.")
  }
}
