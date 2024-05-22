

#' Fit the BDLIM model with all 4 patterns of modification
#'
#' @param y A vector of outcomes
#' @param exposure A matrix of exposures with one row for each individual
#' @param covars A matrix or data.frame of covariates This should not include the grouping factor (see group below). This may include factor variables.
#' @param group A vector of group memberships. This should be a factor variable.
#' @param id An optional vector of individual IDs if there are repeated measures or other groupings that a random intercept should be included for. This must be a factor variable.
#' @param df Degrees of freedom for the weight functions
#' @param nits Number of MCMC iterations.
#' @param nburn Number of MCMC iterations to be discarded as burn in. The default is half if the MCMC iterations. This is only used for WAIC in this function but is passed to summary and plot functions and used there.
#' @param nthin Thinning factors for the MCMC. This is only used for WAIC in this function but is passed to summary and plot functions and used there.
#' @param parallel Logical to use parallel computing for 4 models. If TRUE then the min of 4 and number of cores available will be used.
#' @param family Family of model to be used. Supported options are "gaussian" for a normal/Gaussian linear model and "binomial" for a logistic model.
#'
#' @importFrom parallel makeCluster stopCluster clusterExport parLapply detectCores
#'
#' @return A list of results from each different pattern of modification and model compassion metrics
#' @export
#'
#' @example inst/examples/bdlim_example.R


bdlim4 <- function(y, exposure, covars, group, id=NULL,
                   df, nits, nburn=round(nits/2), nthin=1, parallel=FALSE,
                   family="gaussian"){

  # make sure group is a factor variable
  # this is redundant with bdlim1 but makes the error message clearer to place here also
  if(!is.factor(group)){
    stop("group must be a factor variable.")
  }

  # make sure id is factor
  if(!is.null(id)){
    if(!is.factor(id)){
      stop("id must be a factor variable.")
    }
  }


  # fit each model

  # sequential  - Gaussian
  if(!parallel & toupper(family)=="GAUSSIAN"){
    out <- list()
    message("fitting bw\n")
    out$fit_bw <- bdlim1(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = TRUE, b_free = TRUE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("fitting b\n")
    out$fit_b <- bdlim1(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = FALSE, b_free = TRUE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("fitting w\n")
    out$fit_w <- bdlim1(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = TRUE, b_free = FALSE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("fitting n\n")
    out$fit_n <- bdlim1(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = FALSE, b_free = FALSE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("postprocessing")
  }

  # parallel - Gaussian
  if(parallel & toupper(family)=="GAUSSIAN"){
    message("fitting models in parallel with",min(detectCores(),4),"cores\n")
    cl <- makeCluster(min(detectCores(),4))
    clusterExport(cl, varlist=c("bdlim1","y","exposure","covars","group","id","df","nits","nburn","nthin"), envir = environment())
    out <- parLapply(cl , 1:4, function(mod){
      bdlim1(y = y, exposure = exposure, covars = covars, group = group, id = id,
             w_free = c(TRUE,FALSE,TRUE,FALSE)[mod], b_free =  c(TRUE,TRUE,FALSE,FALSE)[mod],
             df = df, nits = nits, nburn = nburn, nthin = nthin)
    })
    stopCluster(cl)
    names(out) <- c("fit_bw","fit_b","fit_w","fit_n")
  }

  # sequential  - logistic
  if(!parallel & toupper(family)=="BINOMIAL"){
    out <- list()
    message("fitting bw\n")
    out$fit_bw <- bdlim1_logistic(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = TRUE, b_free = TRUE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("fitting b\n")
    out$fit_b <- bdlim1_logistic(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = FALSE, b_free = TRUE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("fitting w\n")
    out$fit_w <- bdlim1_logistic(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = TRUE, b_free = FALSE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("fitting n\n")
    out$fit_n <- bdlim1_logistic(y = y, exposure = exposure, covars = covars, group = group, id = id, w_free = FALSE, b_free = FALSE, df = df, nits = nits, nburn = nburn, nthin = nthin)
    message("postprocessing")
  }

  # parallel - logistic
  if(parallel & toupper(family)=="BINOMIAL"){
    message("fitting models in parallel with",min(detectCores(),4),"cores\n")
    cl <- makeCluster(min(detectCores(),4))
    clusterExport(cl, varlist=c("bdlim1","y","exposure","covars","group","id","df","nits","nburn","nthin"), envir = environment())
    out <- parLapply(cl , 1:4, function(mod){
      bdlim1_logistic(y = y, exposure = exposure, covars = covars, group = group, id = id,
             w_free = c(TRUE,FALSE,TRUE,FALSE)[mod], b_free =  c(TRUE,TRUE,FALSE,FALSE)[mod],
             df = df, nits = nits, nburn = nburn, nthin = nthin)
    })
    stopCluster(cl)
    names(out) <- c("fit_bw","fit_b","fit_w","fit_n")
  }

  message("postprocessing")

  # likelihood comparison
  out$loglik <- data.frame(
    bw=out$fit_bw$loglik,
    b=out$fit_b$loglik,
    w=out$fit_w$loglik,
    n=out$fit_n$loglik
    )

  out$WAIC <- data.frame(
    bw=out$fit_bw$WAIC$WAIC,
    b=out$fit_b$WAIC$WAIC,
    w=out$fit_w$WAIC$WAIC,
    n=out$fit_n$WAIC$WAIC
    )

  # compile results
  out$nits <- nits
  out$nburn <- nburn
  out$nthin <- nthin
  out$call <- match.call()

  class(out) <- "bdlim4"

  return(out)

}
