#' Print Results
#'
#' @param x An object of class bdlim4_0.4.
#' @param ... Not used.
#'
#' @return Assorted model output.
#' @noRd
#' @export
print.bdlim4_0.4 <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)

  cat("\nModification pattern probabilities (higher is better fit):\n")
  mc <- modelcompare(x)
  names(mc)[which.max(mc)] <- paste0("*", names(mc)[which.max(mc)], "*")
  print(mc)

  cat("\nModification pattern WAIC (lower is better fit):\n")
  waic <- x$WAIC
  names(waic)[which.min(waic)] <- paste0("*", names(waic)[which.min(waic)], "*")
  print(waic)

  cat("\nNote: Model probabilities tend to favor more complex models compared to WAIC. It is recomended to use model probabilities for selecting the modification pattern and WAIC to compare degrees of freedom for the weight function.")
}

#' Print Results
#'
#' @param x An object of class bdlim1_0.4.
#' @param ... Not used.
#'
#' @return Assorted model output.
#' @noRd
#' @export
print.bdlim1_0.4 <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)

  cat("\nModification pattern WAIC (lower is better fit):\n")
  waic <- x$WAIC$WAIC
  print(waic)
}
