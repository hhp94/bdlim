#' Print Summary of [bdlim4()]
#'
#' @param x An object of class `summary.bdlim4`.
#' @param ... Not used.
#'
#' @return Assorted model output.
#' @export
print.summary.bdlim4 <- function(x, ...) {
  n <- list(...)$n
  if (is.null(n)) n <- 50

  cat("\nCall:\n")
  print(x$call)

  cat("\nModel fit statistics:\n")
  mc <- x$modelcompare
  bestmodel <- names(mc)[which.max(mc)]
  names(mc)[which.max(mc)] <- paste0("*", names(mc)[which.max(mc)], "*")
  print(mc)

  if (x$family == "binomial" && x$exponentiate) {
    cat("\nParameter estimates from binomial models are exponentiated to be on the odds ratio (OR) scale.")
  }

  cat("\nEstimated cumulative effects:\n")
  print(x$cumulative)

  cat("\nEstimated covariate regression coefficients:\n")
  regcoef <- x$regcoef[!x$regcoef$variable %in% x$variable$Edesign, ]
  print(regcoef, n = n)
  if (!is.null(x$sigma)) {
    cat("\nEstimated residual standard deviation:\n")
    print(x$sigma)
  }
  if (!is.null(x$REsd)) {
    cat("\nEstimated random effect standard deviation is:\n")
    print(x$REsd)
  }
  cat("\nBDLIM fit model =", " \"", x$model, "\"", " on ", x$n, " observations. ", sep = "")
  cat("WAIC is ", x$WAIC[1, bestmodel], ".\n", sep = "")
  cat("\nUse `plot()` for the summary.bdlim4 x to view estimated distributed lag functions. The `dlfun` x in the summary x contains estimates of the lag functions.\n")
  if (nrow(regcoef) > n) {
    warning(paste("Use `print(summary(bdlim4_fit), n = ...)` to print more than", n, "regression coefficients\n"))
  }
  check_mcmc_convergence(x)
}
