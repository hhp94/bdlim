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
#' # Change threshold to silent warnings
#' options(bdlim_rhat_thresh_u = 1.05)
#' options(bdlim_rhat_thresh_l = 0.95)
#' options(bdlim_ess_thresh = 200)
#'
#' # check_mcmc_convergence(summary(bdlim4_fit))
#' }
#'
#' @keywords internal
#' @references https://projecteuclid.org/journals/bayesian-analysis/volume-16/issue-2/Rank-Normalization-Folding-and-Localization--An-Improved-R%CB%86-for/10.1214/20-BA1221.full
check_mcmc_convergence <- function(object) {
  rhat_thresh_u <- getOption("bdlim_rhat_thresh_u", 1.01)
  rhat_thresh_l <- getOption("bdlim_rhat_thresh_l", 0.99)
  stopifnot(
    "`rhat_thresh_l` must be < 1. Change with `options(rhat_thresh_l = 0.99)`." = rhat_thresh_l < 1,
    "`rhat_thresh_u` must be > 1. Change with `options(rhat_thresh_u = 1.01)`." = rhat_thresh_u > 1
  )
  ess_thresh <- getOption("bdlim_ess_thresh", 400)
  stopifnot("`ess_thresh` must be a positive integer. Change with `options(ess_thresh = 400)`." = length(ess_thresh) == 1 && (as.integer(ess_thresh) == ess_thresh))

  if (any(object$MCMC_check$rhat >= rhat_thresh_u)) {
    warning("Some parameters have `rhat` >= ", rhat_thresh_u, ". Increase `nits`, `chains`, or `nthin` to improve convergence.")
  }
  if (any(object$MCMC_check$rhat <= rhat_thresh_l)) {
    warning("Some parameters have `rhat` <= ", rhat_thresh_l, ". Increase `nits`, `chains`, or `nthin` to improve convergence.")
  }
  if (any(object$MCMC_check$ess_bulk < ess_thresh)) {
    warning("Some parameters have `ess_bulk` < ", ess_thresh, ". Increase `nits`, `chains`, or `nthin` to improve effective sample size.")
  }
  if (any(object$MCMC_check$ess_tail < ess_thresh)) {
    warning("Some parameters have `ess_tail` < ", ess_thresh, ". Increase `nits`, `chains`, or `nthin` to improve effective sample size.")
  }
}

