

#' Make orthonomal basis for weight functions
#'
#' @param exposure Matrix of repeated measures of exposure that is n x T where n is the number of observations and T is the number of time points.
#' @param df Degrees of freedom (including intercept) for the natural spline basis to be used.
#'
#' @return A matrix with orthonormal basis expansions of exposure time. The matrix is T x df. These have the span of natural splines with an intercept and df degrees of freedom.
#'
#' @importFrom splines ns
#'
#' @export
#'
#' @examples
#'   B <- makebasis(sbd_bdlim[,paste0("pm25_",1:37)], df=4)

makebasis <- function(exposure,df){
  # make ns basis
  Bns <- ns(seq(1,ncol(exposure)),df=df, intercept=TRUE)

  # smooth exposures with ns basis
  X <-  Bns %*% qr.solve(Bns,t(exposure))

  # SVD of smooth exposures
  svdX <- svd(X)

  # return orthonormalized basis
  return(svdX$u[,1:df])
}

