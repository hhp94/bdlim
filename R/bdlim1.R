

#' Fit the BDLIM model with 1 pattern of modification
#'
#' @param y A vector of outcomes
#' @param exposure A matrix of exposures with one row for each individual
#' @param covars A matrix or data.frame of covariates This should not include the grouping factor (see group below). This may include factor variables.
#' @param group A vector of group memberships. This should be a factor variable.
#' @param id An optional vector of individual IDs if there are repeated measures or other groupings that a random intercept should be included for. This must be a factor variable.
#' @param w_free Logical indicating if the weight functions are shared by all groups (FALSE) or group-specific (TRUE).
#' @param b_free Logical indicating if the effect sizes are shared by all groups (FALSE) or group-specific (TRUE).
#' @param df Degrees of freedom for the weight functions
#' @param nits Number of MCMC iterations.
#' @param nburn Number of MCMC iterations to be discarded as burn in. The default is half if the MCMC iterations. This is only used for WAIC in this function but is passed to summary and plot functions and used there.
#' @param nthin Thinning factors for the MCMC. This is only used for WAIC in this function but is passed to summary and plot functions and used there.
#'
#' @importFrom stats lm sd rnorm rgamma dnorm runif model.matrix
#' @importFrom tidyr drop_na
#' @importFrom dplyr arrange
#' @importFrom LaplacesDemon WAIC
#'
#' @return A list with posteriors of parameters
#' @export

bdlim1 <- function(y, exposure, covars, group, id=NULL, w_free, b_free, df, nits, nburn=round(nits/2), nthin=1){

  # make sure group is a factor variable
  if(!is.factor(group)){
    stop("group must be a factor variable.")
  }

  # make sure covariates have names
  if(is.null(colnames(covars))){
    covars <- as.data.frame(covars)
    colnames(covars) <- paste0("covar",1:ncol(covars))
  }

  # make design matrix for all data except exposures
  # sort by group to make help with MCMC.
  # remove observations with missing values
  # drop unused levels
  alldata <- droplevels(drop_na(cbind(y,group,covars,exposure)), group)
  if(length(y)<nrow(alldata)){
    warning("Dropped",length(y)-nrow(alldata),"observations with missing values.")
  }

  # ADD random effect matrix here
  if(!is.null(id)){
    RE <- model.matrix(~id-1)
    RElocation <- 1:ncol(RE)
    alldata <- cbind(RE,alldata)
    REmodel <- TRUE
    nRE <- ncol(RE)
  }else{
    REmodel <- FALSE
    nRE <- 0
  }


  # dimensions
  n <- nrow(alldata)
  n_groups <- length(unique(alldata$group))
  names_groups <- levels(alldata$group)
  n_times <- ncol(exposure)

  # design matrix for covariates and main effects of group
  mm <- model.matrix(y~.-1, data=alldata)
  design <- mm[,-c(ncol(mm)+1-c(ncol(exposure):1))]

  # exposure matrix
  exposure <- mm[,c(ncol(mm)+1-c(ncol(exposure):1))]

  # basis for weights
  basis <- makebasis(exposure,df=df)

  # preliminary weighted exposures
  # make flat for all groups
  theta <- lm(rep(1/sqrt(37),37)~basis-1)$coef
  w <- drop(basis%*%theta)
  w <- w / sqrt(sum(w^2))
  w <- w * sign(sum(w))

  # starting values for weighted exposures
  # these are the same weightings for all groups
  E <- exposure %*% w


  # replicate if w is group specific
  if(w_free){
    w <- matrix(rep(w,n_groups), n_groups, n_times, byrow = TRUE)
    theta <- matrix(rep(theta,n_groups), n_groups, df, byrow = TRUE)
    n_weight_groups <- n_groups
  }else{
    w <- matrix(w, 1, n_times, byrow = TRUE)
    theta <- matrix(theta, 1, df, byrow = TRUE)
    n_weight_groups <- 1
  }

  # design matrix for weighted exposures
  if(b_free){
    Edesign <- design[,1:n_groups]
    colnames(Edesign) <- paste0("E",names_groups)
  }else{
    Edesign <- matrix(1,n,1)
    colnames(Edesign) <- "E"
  }

  # add weighted exposures to design matrix.
  design <- cbind(design,Edesign*drop(E))
  n_regcoef <- ncol(design)


  # index for groups for weights
  # identifies which rows are in which weight groups
  w_group_ids <- list()
  if(w_free){
    for(j in 1:n_groups){
      w_group_ids[[j]] <- which(design[,j]==1)
    }
  }else{
    w_group_ids[[1]] <- 1:n
  }

  # starting values for other parameters
  sigma <- sd(y)
  REprec <- 0.01

  # place to store results
  regcoef_keep <- matrix(NA, nits, n_regcoef)
  w_keep <- matrix(NA, nits, n_times*n_groups)
  ll_sum_keep <- sigma_keep <- REprec_keep <- rep(NA, nits)

  # iterations to be kept
  # this is only used for waic
  iter_keep <- seq(nburn+1, nits, by=nthin)
  ll_all_keep <- matrix(NA, n, length(iter_keep))


  for(i in 1:nits){

    # update regression coefficients
    V <- t(design) %*% design/(sigma^2)
    diag(V) <- diag(V) + c(rep(REprec,nRE), rep(1/100,n_regcoef-nRE))
    V <- chol2inv(chol(V))
    m <- drop(V %*% (t(design) %*% y)) / (sigma^2)
    regcoef <- drop(m + t(chol(V)) %*% rnorm(n_regcoef))


    # update sigma for Gaussian model
    sigma <- 1/sqrt(rgamma(1,.5 + n/2, .5 + sum((y-design %*% regcoef)^2)/2))

    # update random effect variance if a RE model
    if(REmodel){
      REprec <- rgamma(1,.5 + nRE/2, .5 + sum(regcoef[RElocation]^2)/2)
    }

    for(j in 1:n_weight_groups){

      # log likelihood to start update of theta/w
      ll <- sum(dnorm(y[w_group_ids[[j]]],design[w_group_ids[[j]],] %*% regcoef, sigma, log=TRUE))
      threshold <- ll + log(runif(1))
      ll <- threshold-1 # allows always to start loop


      # vector for ellipse
      nu <- matrix(rnorm(df),1,df)
      eta_max <- eta <- runif(1,0,2*pi)
      eta_min <- eta_max-2*pi

      while(ll<threshold ){

        # proposed coefficients and normalized weights
        theta_prop <- theta[j,]*cos(eta) + nu*sin(eta)
        w[j,] <- drop(basis%*%c(theta_prop))
        w[j,] <- w[j,] / sqrt(sum(w[j,]^2))
        w[j,] <- w[j,] * sign(sum(w[j,]))

        # update weighted exposures for this group
        design[w_group_ids[[j]],colnames(Edesign)] <- as.matrix(Edesign[w_group_ids[[j]],]) * drop(exposure[w_group_ids[[j]],] %*% w[j,])

        # log likelihood
        ll <- sum(dnorm(y[w_group_ids[[j]]],design[w_group_ids[[j]],] %*% regcoef,sigma, log=TRUE))
        # adjust eta in case repeat
        if(eta<0){
          eta_min <- eta
        }else{
          eta_max <- eta
        }
        eta <- runif(1,eta_min,eta_max)
      }

      # update theta (w and design are already updated)
      theta[j,] <- theta_prop
    }

    # save values
    w_keep[i,] <- c(t(w))
    regcoef_keep[i,] <- regcoef
    sigma_keep[i] <- sigma
    REprec_keep[i] <- REprec
    pred_mean_model_scale <- design %*% regcoef
    ll_sum_keep[i] <- sum(dnorm(y,pred_mean_model_scale, sigma, log=TRUE))

    if(i %in% iter_keep){
      ll_all_keep[,which(iter_keep==i)] <- dnorm(y,pred_mean_model_scale, sigma, log=TRUE)
    }
  }

  # format
  colnames(regcoef_keep) <- colnames(design)
  colnames(regcoef_keep)[1:n_groups] <- paste0("intercept",names_groups)
  colnames(w_keep) <- paste0("w_",rep(names_groups,each=n_times),"_",rep(1:n_times,n_groups))

  # summarize posterior for cumulative effect and distributed lag function
  dlfun <- ce <- list()
  for(i in names_groups){
    w_temp <- w_keep[,paste0("w_",i,"_",1:n_times)]
    if(b_free){
      E_temp <- regcoef_keep[,paste0("E",i)]
    }else{
      E_temp <- regcoef_keep[,"E"]
    }
    dlfun[[i]] <- w_temp*E_temp
    ce[[i]] <- rowSums(dlfun[[i]])
  }

  out <- list(
    w = w_keep,
    regcoef = regcoef_keep[,(nRE+1):n_regcoef],
    sigma = sigma_keep,
    loglik = ll_sum_keep,
    names_groups=names_groups,
    n_times=n_times,
    dlfun = dlfun,
    ce = ce,
    WAIC = WAIC(ll_all_keep),
    n = length(y),
    nits=nits,
    nburn=nburn,
    nthin=nthin,
    REmodel=REmodel,
    family="gaussian",
    call=match.call()
  )

  if(REmodel){
    out$RE <- regcoef_keep[,1:nRE]
    out$REsd = 1/sqrt(REprec_keep)
  }

  class(out) <- "bdlim1"

  return(out)

}
