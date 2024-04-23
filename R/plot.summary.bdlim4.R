

#' Plot for Summary pf BDLIM
#'
#' @param x An object of class summary.bdlim4.
#' @param ... Not used.
#' @importFrom ggplot2 ggplot aes_string geom_ribbon geom_line facet_wrap theme_light xlab ylab
#'
#' @return An ggplot2 figure.
#' @export
#'
#' @example inst/examples/plot_example.R

plot.summary.bdlim4 <- function(x, ...){

  p <- ggplot(x$dlfun,  aes_string(x="time", y="mean", ymin="q2.5", ymax="q97.5")) +
    geom_ribbon(color="grey70", fill="grey70") + geom_line() +
    facet_wrap(~group) +
    theme_light() +
    ylab("Estimated distributed lag function") +
    xlab("Exposure time")

  return(p)
}
