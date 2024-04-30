



#' Summary for bdlim4
#'
#' @param object An object of class bdlim4.
#' @param model Pattern of heterogeneity to be printed. If not specified (default) the best fitting model will be used. Options are "n", "b", "w" and "bw" where b indicates the effect sizes are subgroup specific and w indicates the weight functions are subgroups specific.
#' @param ... Other arguments
#'
#' @importFrom stats median quantile
#'
#' @return An object of class summary.bdlim2.
#' @export
#'
#' @example inst/examples/summary_example.R

summary.bdlim4 <- function(object, model=NULL, ...){

  #  iterations to keep.
  iter_keep <- seq(object$nburn+1, object$nits, by=object$nthin)

  out <- list(modelcompare = modelcompare(object),
              WAIC = object$WAIC,
              call = object$call)

  # find best fitting model
  if(is.null(model)){
    model <- names(which.max(out$modelcompare))
  }


  # limit to selected model
  object <- object[[paste0("fit_",model)]]

  #OR scale for binomial
  if(object$family=="binomial"){
    object$ce <- lapply(object$ce, exp)
    object$dlfun <- lapply(object$dlfun, exp)
    object$regcoef <- exp(object$regcoef)
  }

  # summarize cumulative effect
  ce <- data.frame(
    group = object$names_groups,
    mean = sapply(object$ce, function(x) mean(x[iter_keep])),
    median = sapply(object$ce, function(x) median(x[iter_keep])),
    sd = sapply(object$ce, function(x) sd(x[iter_keep])),
    q2.5 = sapply(object$ce, function(x) quantile(x[iter_keep], 0.025)),
    q97.5 = sapply(object$ce, function(x) quantile(x[iter_keep], 0.975)),
    pr_gr0 = sapply(object$ce, function(x) mean(x[iter_keep]>0))
    )
  row.names(ce) <- NULL
  out$cumulative <- ce

  # summarize distributed lag function
  dlfun <- data.frame(
    group = rep(object$names_groups, each=object$n_times),
    time = rep(1:object$n_times, length(object$names_groups)),
    mean = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,mean))),
    median = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,median))),
    sd = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,sd))),
    q2.5 = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,quantile,0.025))),
    q97.5 = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,quantile,0.975))),
    pr_gr0 = c(sapply(object$dlfun, function(x) apply(x[iter_keep,]>0,2,mean)))
  )
  row.names(dlfun) <- NULL

  out$dlfun <- dlfun



  # summarize covariates regression coefficients
  regcoef <- data.frame(
    name = colnames(object$regcoef),
    mean = apply(object$regcoef, 2, function(x) mean(x[iter_keep])),
    median = apply(object$regcoef, 2, function(x) median(x[iter_keep])),
    sd = apply(object$regcoef, 2, function(x) sd(x[iter_keep])),
    q2.5 = apply(object$regcoef, 2, function(x) quantile(x[iter_keep], 0.025)),
    q97.5 = apply(object$regcoef, 2, function(x) quantile(x[iter_keep], 0.975)),
    pr_gr0 = apply(object$regcoef, 2, function(x) mean(x[iter_keep]>0))
  )
  row.names(regcoef) <- NULL
  out$regcoef <- regcoef

  # summarize covariates regression coefficients
  if(object$family=="gaussian"){
  sigma <- data.frame(
    name = "sigma",
    mean = mean(object$sigma[iter_keep]),
    median = median(object$sigma[iter_keep]),
    sd = median(object$sigma[iter_keep]),
    q2.5 = quantile(object$sigma[iter_keep], 0.025),
    q97.5 = quantile(object$sigma[iter_keep], 0.975)
  )
  row.names(sigma) <- NULL
  out$sigma <- sigma
  }

  # summarize covariates regression coefficients
  if(object$REmodel){
    REsd <- data.frame(
      name = "REsd",
      mean = mean(object$REsd[iter_keep]),
      median = median(object$REsd[iter_keep]),
      sd = median(object$REsd[iter_keep]),
      q2.5 = quantile(object$REsd[iter_keep], 0.025),
      q97.5 = quantile(object$REsd[iter_keep], 0.975)
    )
    row.names(REsd) <- NULL
    out$REsd <- REsd
    out$nRElevels <- ncol(object$RE)
  }

  out$names_groups <- object$names_groups

  out$n <- object$n

  class(out) <- "summary.bdlim4"

  out$family <- object$family

  return(out)
}




#' Summary for bdlim1
#'
#' @param object An object of class bdlim1.
#' @param ... Not used.
#'
#' @importFrom stats median quantile
#'
#' @return An object of class summary.bdlim2.
#' @export
#'

summary.bdlim1 <- function(object, ...){

  if(object$call$nburn=="nburn"){
    stop("Use summary of the bdlim4 object. Use option `model' to get a specific bdlim1 model.")
  }

  # check which iterations to keep.
  if(is.null(object$call$nburn)){
    object$call$nburn <- round(object$call$nits/2)
  }
  if(is.null(object$call$nthin)){
    object$call$nthin <- 1
  }
  iter_keep <- seq(object$call$nburn+1, object$call$nits, by=object$call$nthin)

  out <- list(WAIC = object$WAIC$WAIC,
              call = object$call)


  #OR scale for binomial
  if(object$family=="binomial"){
    object$ce <- exp(object$ce)
    object$dlfun <- exp(object$dlfun)
    object$regcoef <- exp(object$regcoef)
  }

  # summarize cumulative effect
  ce <- data.frame(
    group = object$names_groups,
    mean = sapply(object$ce, function(x) mean(x[iter_keep])),
    median = sapply(object$ce, function(x) median(x[iter_keep])),
    sd = sapply(object$ce, function(x) sd(x[iter_keep])),
    q2.5 = sapply(object$ce, function(x) quantile(x[iter_keep], 0.025)),
    q97.5 = sapply(object$ce, function(x) quantile(x[iter_keep], 0.975)),
    pr_gr0 = sapply(object$ce, function(x) mean(x[iter_keep]>0))
  )
  row.names(ce) <- NULL
  out$cumulative <- ce

  # summarize distributed lag function
  dlfun <- data.frame(
    group = rep(object$names_groups, each=object$n_times),
    time = rep(1:object$n_times, length(object$names_groups)),
    mean = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,mean))),
    median = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,median))),
    sd = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,sd))),
    q2.5 = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,quantile,0.025))),
    q97.5 = c(sapply(object$dlfun, function(x) apply(x[iter_keep,],2,quantile,0.975))),
    pr_gr0 = c(sapply(object$dlfun, function(x) apply(x[iter_keep,]>0,2,mean)))
  )
  row.names(dlfun) <- NULL

  out$dlfun <- dlfun



  # summarize covariates regression coefficients
  regcoef <- data.frame(
    name = colnames(object$regcoef),
    mean = apply(object$regcoef, 2, function(x) mean(x[iter_keep])),
    median = apply(object$regcoef, 2, function(x) median(x[iter_keep])),
    sd = apply(object$regcoef, 2, function(x) sd(x[iter_keep])),
    q2.5 = apply(object$regcoef, 2, function(x) quantile(x[iter_keep], 0.025)),
    q97.5 = apply(object$regcoef, 2, function(x) quantile(x[iter_keep], 0.975)),
    pr_gr0 = apply(object$regcoef, 2, function(x) mean(x[iter_keep]>0))
  )
  row.names(regcoef) <- NULL
  out$regcoef <- regcoef

  # summarize covariates regression coefficients
  if(object$family=="gaussian"){
    sigma <- data.frame(
      name = "sigma",
      mean = mean(object$sigma[iter_keep]),
      median = median(object$sigma[iter_keep]),
      sd = median(object$sigma[iter_keep]),
      q2.5 = quantile(object$sigma[iter_keep], 0.025),
      q97.5 = quantile(object$sigma[iter_keep], 0.975)
    )
    row.names(sigma) <- NULL
    out$sigma <- sigma
  }

  # summarize covariates regression coefficients
  if(object$REmodel){
    REsd <- data.frame(
      name = "REsd",
      mean = mean(object$REsd[iter_keep]),
      median = median(object$REsd[iter_keep]),
      sd = median(object$REsd[iter_keep]),
      q2.5 = quantile(object$REsd[iter_keep], 0.025),
      q97.5 = quantile(object$REsd[iter_keep], 0.975)
    )
    row.names(REsd) <- NULL
    out$REsd <- REsd
    out$nRElevels <- ncol(object$RE)
  }

  out$names_groups <- object$names_groups

  out$n <- object$n

  out$family <- object$family

  class(out) <- "summary.bdlim1"

  return(out)
}
