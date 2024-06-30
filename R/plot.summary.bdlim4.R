#' Plot for Summary of BDLIM
#'
#' @details
#' Pass `FALSE` to `continuous_weight` and points and error bars will be used instead.
#'
#' @param x An object of class `summary.bdlim4`.
#' @param ... Not used.
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line facet_wrap theme_light xlab ylab geom_pointrange
#'
#' @return A ggplot2 figure.
#' @importFrom rlang .data
#' @export
#'
#' @example inst/examples/plot_example.R
plot.summary.bdlim4 <- function(x, ...) {
  continuous_weight <- list(...)$continuous_weight
  if (is.null(continuous_weight)) continuous_weight <- TRUE
  stopifnot(is.logical(continuous_weight) && length(continuous_weight) == 1)

  # Check if the required columns are present
  required_cols <- c("time", "mean", "q2.5", "q97.5", "group")
  missing_cols <- setdiff(required_cols, names(x$dlfun))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from the x$dlfun: ", paste(missing_cols, collapse = ", "))
  }

  p <- ggplot(x$dlfun, aes(x = .data[["time"]], y = .data[["mean"]], ymin = .data[["q2.5"]], ymax = .data[["q97.5"]]))

  if (continuous_weight) {
    p <- p +
      geom_ribbon(fill = "grey70", alpha = 0.5) +
      geom_line()
  } else {
    p <- p +
      geom_pointrange()
  }

  p <- p +
    facet_wrap(~group) +
    theme_light() +
    ylab("Estimated distributed lag function") +
    xlab("Exposure time")

  return(p)
}
