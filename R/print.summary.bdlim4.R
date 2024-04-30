

#' Print Summary of bdlim4
#'
#' @param x An object of class summary.bdlim4.
#' @param ... Not used.
#'
#' @return Assorted model output.
#' @export
#'

print.summary.bdlim4 <- function(x, ...){


  cat("\nCall:\n")
  print(x$call)

  cat("\n\nModel fit statistics:\n")
  mc <- x$modelcompare
  bestmodel <- names(mc)[which.max(mc)]
  names(mc)[which.max(mc)] <- paste0("*",names(mc)[which.max(mc)],"*")
  print(mc)

  if(x$family=="binomial"){
    cat("\nParameter estimates from binomial models are exponentiated to be on the odds ratio (OR) scale.")
  }

  cat("\n\nEstimated cumulative effects:\n")
  print(x$cumulative, row.names = FALSE)


  cat("\n\nEstimated covariate regression coefficients:\n")
  print(x$regcoef[!(x$regcoef$name %in% c("E",paste0("E",x$names_groups))),], row.names = FALSE)

  cat("\n\nBDLIM fit on",x$n,"observations. ")
  if(!is.null(x$sigma)){
    cat("Estimated residual standard deviation is ", round(x$sigma[1,"mean"],3) , " (", round(x$sigma[1,"q2.5"],3) , "," , round(x$sigma[1,"q97.5"],3) , "). ", sep="")
  }
  if(!is.null(x$REsd)){
    cat("Estimated random effect standard deviation is ", round(x$REsd[1,"mean"],3) , " (", round(x$REsd[1,"q2.5"],3) , "," , round(x$REsd[1,"q97.5"],3) , ") with ",x$nRElevels," levels. ", sep="")
  }
  cat("WAIC is ", x$WAIC[1,bestmodel],".", sep="")

  cat("\n\nUse `plot(); for the summary.bdlim4 object to view estimated distributed lag functions. The `dlfun' object in the summary object contains estimates of the lag functions.")

}

