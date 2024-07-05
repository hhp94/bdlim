#' bdlim1.1 <- function(y, exposure, covars, group, id, w_free, b_free, df, nits, nburn, nthin, chains, family, loglik_all) {
#'   # switch between family
#'   bdlim1_fit <- switch(family,
#'                        gaussian = bdlim1_gaussian,
#'                        binomial = bdlim1_logistic,
#'                        stop("Unsupported Family")
#'   )
#'
#'   # bind all data into one data.frame.
#'   # Because we no longer allow NA, we don't need to add exposure to `alldata`.
#'   # group has to be the first factor, otherwise stats::model.matrix won't create a
#'   # term for each group
#'   alldata <- data.frame(y = y, group = group)
#'   if (!is.null(covars)) {
#'     alldata <- droplevels(cbind(alldata, covars))
#'   }
#'
#'   # Add random effect matrix here
#'   if (!is.null(id)) {
#'     id <- droplevels(id)
#'     RE <- stats::model.matrix(~ id - 1)
#'     nRE <- ncol(RE)
#'     colnames(RE) <- paste0(".id", seq_len(ncol(RE)))
#'     alldata <- cbind(RE, alldata)
#'     RE_loc <- 1:nRE
#'     REmodel <- TRUE
#'   } else {
#'     RE <- NULL
#'     nRE <- 0
#'     RE_loc <- 0
#'     REmodel <- FALSE
#'   }
#'
#'   # dimensions
#'   n <- nrow(alldata)
#'   n_groups <- length(levels(alldata$group))
#'
#'   # Calculate group_loc
#'   group_loc <- (nRE + 1):(nRE + n_groups)
#'
#'   names_groups <- make.names(levels(alldata$group), unique = TRUE, allow_ = FALSE)
#'   n_times <- ncol(exposure)
#'
#'   # design matrix for covariates and main effects of group
#'   design <- stats::model.matrix(y ~ . - 1, data = alldata)
#'
#'   # basis for weights
#'   basisObj <- makebasis(exposure, df = df)
#'   basis <- basisObj$u
#'
#'   # preliminary weighted exposures
#'   # make flat for all groups
#'   theta <- stats::lm(rep(1 / sqrt(n_times), n_times) ~ basis - 1)$coef
#'   w <- drop(basis %*% theta)
#'   w <- w / sqrt(sum(w^2))
#'   w <- w * sign(sum(w))
#'
#'   # starting values for weighted exposures
#'   # these are the same weightings for all groups
#'   .E <- exposure %*% w
#'
#'   # replicate if w is group specific
#'   if (w_free) {
#'     w <- matrix(rep(w, n_groups), n_groups, n_times, byrow = TRUE)
#'     theta <- matrix(rep(theta, n_groups), n_groups, df, byrow = TRUE)
#'     n_weight_groups <- n_groups
#'   } else {
#'     w <- matrix(w, 1, n_times, byrow = TRUE)
#'     theta <- matrix(theta, 1, df, byrow = TRUE)
#'     n_weight_groups <- 1
#'   }
#'
#'   # design matrix for weighted exposures
#'   if (b_free) {
#'     Edesign <- design[, group_loc]
#'     colnames(Edesign) <- paste0(".E", names_groups)
#'   } else {
#'     Edesign <- matrix(1, n, 1)
#'     colnames(Edesign) <- ".E"
#'   }
#'
#'   # add weighted exposures to design matrix.
#'   design <- cbind(design, Edesign * drop(.E))
#'   n_regcoef <- ncol(design)
#'
#'   # index for groups for weights identifies which rows are in which weight groups
#'   group_indices <- apply(Edesign, 2, \(x){which(x == 1, useNames = FALSE)}, simplify = FALSE)
#'
#'   ## RE precision
#'   REprec <- 0.01
#'
#'   ## iterations to be kept
#'   ## this is only used for waic
#'   iter_keep <- seq(nburn + 1, nits, by = nthin)
#'   ll_all_keep <- matrix(NA, n, length(iter_keep))
#'
#'   # format
#'   # if chains == 1, switch to lapply to 100% reproduce 0.4 version fit
#'   if (chains == 1) {
#'     chain_fit <- lapply
#'   } else {
#'     chain_fit <- future.apply::future_lapply
#'   }
#'
#'   w_list <- design_list <- y_list <- Edesign_list <- theta_list <- list()
#'   for(i in seq_along(group_indices)) {
#'     w_list <- w[i, ]
#'     theta_list <- theta[i, ]
#'     design_list <- design[group_indices[[i]], ]
#'     y_list <- y[group_indices[[i]]]
#'     Edesign_list <- Edesign[group_indices[[i]], ]
#'   }
#'
#'   mcmc_params <- list(
#'     w_list = w_list,
#'     theta_list = theta_list,
#'     design_list = design_list,
#'     y_list <- y_list,
#'     Edesign_list <- Edesign_list
#'   )
#'
#'   return(out)
#' }
#'
#' #' Combine Multiple Chains
#' #'
#' #' Combine MCMC Chains into a `posterior` object. The log likelihood matrix is an N*S matrix
#' #' where N is the number of rows and S is the number of draws.
#' #'
#' #' @param out A `bdlim1_gaussian` or `bdlim1_logistic` fit.
#' #' @keywords internal
#' #' @noRd
#' process_chains <- function(out) {
#'   # Get the names of the elements in each chain excluding `ll_all_keep`
#'   param <- setdiff(names(out[[1]]), "ll_all_keep")
#'
#'   # Convert each chain to a consistent format for other MCMC packages
#'   draws <- posterior::as_draws_list(lapply(out, function(x) x[param]))
#'   draws <- posterior::as_draws_array(draws)
#'
#'   # Combine the log likelihood draws matrix for LOO and WAIC calculations
#'   ll_all_keep <- do.call(cbind, lapply(out, function(x) x[["ll_all_keep"]]))
#'
#'   return(list(draws = draws, ll_all_keep = ll_all_keep))
#' }
#'
#' #' @keywords internal
#' #' @noRd
#' bdlim1_gaussian <- function(
#'     y,
#'     w,
#'     nits,
#'     design,
#'     nRE,
#'     REprec,
#'     n_regcoef,
#'     REmodel,
#'     RE_loc,
#'     n_weight_groups,
#'     w_group_ids,
#'     theta,
#'     df,
#'     basis,
#'     Edesign,
#'     exposure,
#'     w_keep,
#'     regcoef_keep,
#'     REprec_keep,
#'     ll_sum_keep,
#'     ll_all_keep,
#'     names_groups,
#'     n_times,
#'     b_free,
#'     n_groups,
#'     iter_keep) {
#'   # starting values specific for `bdlim1_gaussian`
#'   n <- length(y)
#'   sigma <- stats::sd(y)
#'   sigma_keep <- rep(NA, nits)
#'   pred_mean_model_scale <- NA
#'
#'   for (i in 1:nits) {
#'     # update regression coefficients
#'     V <- t(design) %*% design / (sigma^2)
#'     diag(V) <- diag(V) + c(rep(REprec, nRE), rep(1 / 100, n_regcoef - nRE))
#'     V <- chol2inv(chol(V))
#'     m <- drop(V %*% (t(design) %*% y)) / (sigma^2)
#'     regcoef <- drop(m + t(chol(V)) %*% stats::rnorm(n_regcoef))
#'
#'     # update sigma for Gaussian model
#'     sigma <- 1 / sqrt(stats::rgamma(1, .5 + n / 2, .5 + sum((y - design %*% regcoef)^2) / 2))
#'
#'     # update random effect variance if a RE model
#'     if (REmodel) {
#'       REprec <- stats::rgamma(1, .5 + nRE / 2, .5 + sum(regcoef[RE_loc]^2) / 2)
#'     }
#'
#'     for (j in 1:n_weight_groups) {
#'       # log likelihood to start update of theta/w
#'       ll <- sum(stats::dnorm(y[w_group_ids[[j]]], design[w_group_ids[[j]], ] %*% regcoef, sigma, log = TRUE))
#'       threshold <- ll + log(stats::runif(1))
#'       ll <- threshold - 1 # allows always to start loop
#'
#'       # vector for ellipse
#'       nu <- matrix(stats::rnorm(df), 1, df)
#'       eta_max <- eta <- stats::runif(1, 0, 2 * pi)
#'       eta_min <- eta_max - 2 * pi
#'
#'       while (ll < threshold) {
#'         # proposed coefficients and normalized weights
#'         theta_prop <- theta[j, ] * cos(eta) + nu * sin(eta)
#'         w[j, ] <- drop(basis %*% c(theta_prop))
#'         w[j, ] <- w[j, ] / sqrt(sum(w[j, ]^2))
#'         w[j, ] <- w[j, ] * sign(sum(w[j, ]))
#'
#'         # update weighted exposures for this group
#'         design[w_group_ids[[j]], colnames(Edesign)] <- as.matrix(Edesign[w_group_ids[[j]], ]) * drop(exposure[w_group_ids[[j]], ] %*% w[j, ])
#'
#'         # log likelihood
#'         ll <- sum(stats::dnorm(y[w_group_ids[[j]]], design[w_group_ids[[j]], ] %*% regcoef, sigma, log = TRUE))
#'         # adjust eta in case repeat
#'         if (eta < 0) {
#'           eta_min <- eta
#'         } else {
#'           eta_max <- eta
#'         }
#'         eta <- stats::runif(1, eta_min, eta_max)
#'       }
#'
#'       # update theta (w and design are already updated)
#'       theta[j, ] <- theta_prop
#'     }
#'
#'     # save values
#'     w_keep[i, ] <- c(t(w))
#'     regcoef_keep[i, ] <- regcoef
#'     sigma_keep[i] <- sigma
#'     if (REmodel) {
#'       REprec_keep[i] <- REprec
#'     }
#'     pred_mean_model_scale <- design %*% regcoef
#'     # calculate log-likelihood once
#'     ll_all <- stats::dnorm(y, pred_mean_model_scale, sigma, log = TRUE)
#'     ll_sum_keep[i] <- sum(ll_all)
#'     if (i %in% iter_keep) {
#'       ll_all_keep[, which(iter_keep == i)] <- ll_all
#'     }
#'   }
#'
#'   # Calculating beta*w and cumulative effect for each group
#'   dlfun <- ce <- list()
#'   for (i in names_groups) {
#'     w_cols <- paste0("w_", i, "_", 1:n_times)
#'     if (b_free) {
#'       dlfun[[i]] <- w_keep[, w_cols] * regcoef_keep[, paste0(".E", i)]
#'     } else {
#'       dlfun[[i]] <- w_keep[, w_cols] * regcoef_keep[, ".E"]
#'     }
#'     colnames(dlfun[[i]]) <- paste0(".E", w_cols)
#'     ce[[i]] <- rowSums(dlfun[[i]])
#'   }
#'   names(ce) <- paste0("ce", "_", names_groups)
#'
#'   out <- c(
#'     asplit(w_keep, 2),
#'     asplit(regcoef_keep[, (nRE + 1):n_regcoef], 2),
#'     list(sigma = sigma_keep),
#'     asplit(do.call(cbind, dlfun), 2),
#'     asplit(do.call(cbind, ce), 2),
#'     list(loglik = ll_sum_keep),
#'     list(ll_all_keep = ll_all_keep)
#'   )
#'
#'   if (REmodel) {
#'     out <- c(
#'       out,
#'       asplit(regcoef_keep[, 1:nRE], 2),
#'       list(REsd = 1 / sqrt(REprec_keep))
#'     )
#'   }
#'
#'   return(out)
#' }
