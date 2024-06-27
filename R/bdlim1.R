#' Fit the BDLIM model with 1 Pattern of Modification
#'
#' @inheritParams bdlim4
#' @param w_free Logical indicating if the weight functions are shared by all groups (FALSE) or group-specific (TRUE).
#' @param b_free Logical indicating if the effect sizes are shared by all groups (FALSE) or group-specific (TRUE).
#'
#' @importFrom stats lm sd rnorm rgamma dnorm runif model.matrix
#' @importFrom LaplacesDemon WAIC
#'
#' @return A list with posteriors of parameters
#' @export
bdlim1 <- function(y, exposure, covars, group, id, w_free, b_free, df, nits, nburn, nthin, chains, family) {
  # switch between family
  bdlim1_fit <- switch(family,
    gaussian = bdlim1_gaussian,
    binomial = bdlim1_logistic,
    stop("Unsupported Family")
  )

  # bind all data into one data.frame.
  # Because we no longer allow NA, we don't need to add exposure to `alldata`.
  # group has to be the first factor, otherwise model.matrix won't create a
  # term for each group
  alldata <- droplevels(cbind(data.frame(y = y, group = group), covars))

  # add random effect matrix here
  if (!is.null(id)) {
    id <- droplevels(id)
    RE <- model.matrix(~ id - 1)
    colnames(RE) <- paste("RE", "int", seq_len(ncol(RE)), sep = "_")
    RElocation <- 1:ncol(RE)
    alldata <- cbind(RE, alldata)
    REmodel <- TRUE
    nRE <- ncol(RE)
  } else {
    RE <- NULL
    RElocation <- NULL
    REmodel <- FALSE
    nRE <- 0
  }

  # dimensions
  n <- nrow(alldata)
  n_groups <- length(unique(alldata$group))
  names_groups <- levels(alldata$group)
  n_times <- ncol(exposure)

  # design matrix for covariates and main effects of group
  design <- model.matrix(y ~ . - 1, data = alldata)

  # basis for weights
  basis <- makebasis(exposure, df = df)

  # preliminary weighted exposures
  # make flat for all groups
  theta <- lm(rep(1 / sqrt(n_times), n_times) ~ basis - 1)$coef
  w <- drop(basis %*% theta)
  w <- w / sqrt(sum(w^2))
  w <- w * sign(sum(w))

  # starting values for weighted exposures
  # these are the same weightings for all groups
  E <- exposure %*% w

  # replicate if w is group specific
  if (w_free) {
    w <- matrix(rep(w, n_groups), n_groups, n_times, byrow = TRUE)
    theta <- matrix(rep(theta, n_groups), n_groups, df, byrow = TRUE)
    n_weight_groups <- n_groups
  } else {
    w <- matrix(w, 1, n_times, byrow = TRUE)
    theta <- matrix(theta, 1, df, byrow = TRUE)
    n_weight_groups <- 1
  }

  # design matrix for weighted exposures
  if (b_free) {
    Edesign <- design[, 1:n_groups]
    colnames(Edesign) <- paste0("E", names_groups)
  } else {
    Edesign <- matrix(1, n, 1)
    colnames(Edesign) <- "E"
  }

  # add weighted exposures to design matrix.
  design <- cbind(design, Edesign * drop(E))
  n_regcoef <- ncol(design)

  # index for groups for weights
  # identifies which rows are in which weight groups
  w_group_ids <- list()
  if (w_free) {
    for (j in 1:n_groups) {
      w_group_ids[[j]] <- which(design[, j] == 1)
    }
  } else {
    w_group_ids[[1]] <- 1:n
  }

  ## RE precision
  REprec <- if(REmodel) 0.01 else NULL

  ## place to store results
  regcoef_keep <- matrix(NA, nits, n_regcoef)
  w_keep <- matrix(NA, nits, n_times * n_groups)
  ll_sum_keep <- REprec_keep <- rep(NA, nits)

  ## iterations to be kept
  ## this is only used for waic
  iter_keep <- seq(nburn + 1, nits, by = nthin)
  ll_all_keep <- matrix(NA, n, length(iter_keep))

  # format
  colnames(regcoef_keep) <- colnames(design)
  colnames(regcoef_keep)[1:n_groups] <- paste0("intercept", names_groups)
  colnames(w_keep) <- paste0("w_", rep(names_groups, each = n_times), "_", rep(1:n_times, n_groups))

  out <- future.apply::future_lapply(
    seq_len(chains),
    \(x) {
      bdlim1_fit(
        y = y,
        w = w,
        nits = nits,
        design = design,
        nRE = nRE,
        REprec = REprec,
        n_regcoef = n_regcoef,
        REmodel = REmodel,
        RElocation = RElocation,
        n_weight_groups = n_weight_groups,
        w_group_ids = w_group_ids,
        theta = theta,
        df = df,
        basis = basis,
        Edesign = Edesign,
        exposure = exposure,
        w_keep = w_keep,
        regcoef_keep = regcoef_keep,
        REprec_keep = REprec_keep,
        ll_sum_keep = ll_sum_keep,
        ll_all_keep = ll_all_keep,
        names_groups = names_groups,
        n_times = n_times,
        b_free = b_free,
        n_groups = n_groups,
        iter_keep = iter_keep
      )
    },
    future.seed = TRUE
  )

  out <- process_chains(out)

  out <- c(
    out,
    list(
      WAIC = WAIC(out$ll_all_keep),
      n = n,
      nits = nits,
      nburn = nburn,
      nthin = nthin,
      REmodel = REmodel,
      family = family,
      names_groups = names_groups,
      n_times = n_times,
      REmodel = REmodel,
      call = match.call()
    )
  )

  # calculate posterior for cumulative effect and distributed lag function
  # dlfun <- ce <- list()
  # for (i in names_groups) {
  #   w_temp <- w_keep[, paste0("w_", i, "_", 1:n_times)]
  #   if (b_free) {
  #     E_temp <- regcoef_keep[, paste0("E", i)]
  #   } else {
  #     E_temp <- regcoef_keep[, "E"]
  #   }
  #   dlfun[[i]] <- w_temp * E_temp
  #   ce[[i]] <- rowSums(dlfun[[i]])
  # }

  class(out) <- "bdlim1"

  return(out)
}

process_chains <- function(out) {
  # Get the names of the elements in each chain
  element_names <- names(out[[1]])

  # Stacking the chains into one list. Basically a nested loops that
  # add the same elements of each chains into one list
  all_chains <- lapply(element_names, function(name) {
    lapply(out, function(x) { x[[name]] } )
  })

  # Assign the names to the transformed list
  names(all_chains) <- element_names
  return(all_chains)
}

bdlim1_gaussian <- function(
    y,
    w,
    nits,
    design,
    nRE,
    REprec,
    n_regcoef,
    REmodel,
    RElocation,
    n_weight_groups,
    w_group_ids,
    theta,
    df,
    basis,
    Edesign,
    exposure,
    w_keep,
    regcoef_keep,
    REprec_keep,
    ll_sum_keep,
    ll_all_keep,
    names_groups,
    n_times,
    b_free,
    n_groups,
    iter_keep) {
  # starting values specific for `bdlim1_gaussian`
  n <- length(y)
  sigma <- sd(y)
  sigma_keep <- rep(NA, nits)
  pred_mean_model_scale <- NA

  for (i in 1:nits) {
    # update regression coefficients
    V <- t(design) %*% design / (sigma^2)
    diag(V) <- diag(V) + c(rep(REprec, nRE), rep(1 / 100, n_regcoef - nRE))
    V <- chol2inv(chol(V))
    m <- drop(V %*% (t(design) %*% y)) / (sigma^2)
    regcoef <- drop(m + t(chol(V)) %*% rnorm(n_regcoef))

    # update sigma for Gaussian model
    sigma <- 1 / sqrt(rgamma(1, .5 + n / 2, .5 + sum((y - design %*% regcoef)^2) / 2))

    # update random effect variance if a RE model
    if (REmodel) {
      REprec <- rgamma(1, .5 + nRE / 2, .5 + sum(regcoef[RElocation]^2) / 2)
    }

    for (j in 1:n_weight_groups) {
      # log likelihood to start update of theta/w
      ll <- sum(dnorm(y[w_group_ids[[j]]], design[w_group_ids[[j]], ] %*% regcoef, sigma, log = TRUE))
      threshold <- ll + log(runif(1))
      ll <- threshold - 1 # allows always to start loop

      # vector for ellipse
      nu <- matrix(rnorm(df), 1, df)
      eta_max <- eta <- runif(1, 0, 2 * pi)
      eta_min <- eta_max - 2 * pi

      while (ll < threshold) {
        # proposed coefficients and normalized weights
        theta_prop <- theta[j, ] * cos(eta) + nu * sin(eta)
        w[j, ] <- drop(basis %*% c(theta_prop))
        w[j, ] <- w[j, ] / sqrt(sum(w[j, ]^2))
        w[j, ] <- w[j, ] * sign(sum(w[j, ]))

        # update weighted exposures for this group
        design[w_group_ids[[j]], colnames(Edesign)] <- as.matrix(Edesign[w_group_ids[[j]], ]) * drop(exposure[w_group_ids[[j]], ] %*% w[j, ])

        # log likelihood
        ll <- sum(dnorm(y[w_group_ids[[j]]], design[w_group_ids[[j]], ] %*% regcoef, sigma, log = TRUE))
        # adjust eta in case repeat
        if (eta < 0) {
          eta_min <- eta
        } else {
          eta_max <- eta
        }
        eta <- runif(1, eta_min, eta_max)
      }

      # update theta (w and design are already updated)
      theta[j, ] <- theta_prop
    }

    # save values
    w_keep[i, ] <- c(t(w))
    regcoef_keep[i, ] <- regcoef
    sigma_keep[i] <- sigma
    if (REmodel) {
      REprec_keep[i] <- REprec
    }
    pred_mean_model_scale <- design %*% regcoef
    ll_sum_keep[i] <- sum(dnorm(y, pred_mean_model_scale, sigma, log = TRUE))

    if (i %in% iter_keep) {
      ll_all_keep[, which(iter_keep == i)] <- dnorm(y, pred_mean_model_scale, sigma, log = TRUE)
    }
  }

  out <- list(
    w = w_keep,
    regcoef = regcoef_keep[, (nRE + 1):n_regcoef],
    sigma = sigma_keep,
    loglik = ll_sum_keep,
    ll_all_keep = ll_all_keep
  )

  if (REmodel) {
    out$RE <- regcoef_keep[, 1:nRE]
    out$REsd <- 1 / sqrt(REprec_keep)
  }

  return(out)
}

bdlim1_logistic <- function(
    y,
    w,
    nits,
    design,
    nRE,
    REprec,
    n_regcoef,
    REmodel,
    RElocation,
    n_weight_groups,
    w_group_ids,
    theta,
    df,
    basis,
    Edesign,
    exposure,
    w_keep,
    regcoef_keep,
    REprec_keep,
    ll_sum_keep,
    ll_all_keep,
    names_groups,
    n_times,
    b_free,
    n_groups,
    iter_keep) {
  n <- length(y)
  # set linear predictor to 0 for starting values
  pred_mean_model_scale <- rep(0, n)

  for (i in 1:nits) {
    # omega from PG augmentation
    w_pg <- rpg(n, 1, pred_mean_model_scale)

    # update regression coefficients
    design_w <- t(scale(t(design), 1 / sqrt(w_pg), center = FALSE))
    V <- t(design_w) %*% design_w
    diag(V) <- diag(V) + c(rep(REprec, nRE), rep(1 / 100, n_regcoef - nRE))
    V <- chol2inv(chol(V))
    m <- drop(V %*% (t(design) %*% (y - .5)))
    regcoef <- drop(m + t(chol(V)) %*% rnorm(n_regcoef))

    # update random effect variance if a RE model
    if (REmodel) {
      REprec <- rgamma(1, .5 + nRE / 2, .5 + sum(regcoef[RElocation]^2) / 2)
    }

    for (j in 1:n_weight_groups) {
      # log likelihood to start update of theta/w
      ll <- sum(dbinom(y[w_group_ids[[j]]], 1, 1 / (1 + exp(-design[w_group_ids[[j]], ] %*% regcoef)), log = TRUE))
      threshold <- ll + log(runif(1))
      ll <- threshold - 1 # allows always to start loop

      # vector for ellipse
      nu <- matrix(rnorm(df), 1, df)
      eta_max <- eta <- runif(1, 0, 2 * pi)
      eta_min <- eta_max - 2 * pi

      while (ll < threshold) {
        # proposed coefficients and normalized weights
        theta_prop <- theta[j, ] * cos(eta) + nu * sin(eta)
        w[j, ] <- drop(basis %*% c(theta_prop))
        w[j, ] <- w[j, ] / sqrt(sum(w[j, ]^2))
        w[j, ] <- w[j, ] * sign(sum(w[j, ]))

        # update weighted exposures for this group
        design[w_group_ids[[j]], colnames(Edesign)] <- as.matrix(Edesign[w_group_ids[[j]], ]) * drop(exposure[w_group_ids[[j]], ] %*% w[j, ])

        # log likelihood
        ll <- sum(dbinom(y[w_group_ids[[j]]], 1, 1 / (1 + exp(-design[w_group_ids[[j]], ] %*% regcoef)), log = TRUE))
        # adjust eta in case repeat
        if (eta < 0) {
          eta_min <- eta
        } else {
          eta_max <- eta
        }
        eta <- runif(1, eta_min, eta_max)
      }

      # update theta (w and design are already updated)
      theta[j, ] <- theta_prop
    }

    # save values
    w_keep[i, ] <- c(t(w))
    regcoef_keep[i, ] <- regcoef
    if (REmodel) {
      REprec_keep[i] <- REprec
    }
    pred_mean_model_scale <- design %*% regcoef
    ll_sum_keep[i] <- sum(dbinom(y, 1, 1 / (1 + exp(-pred_mean_model_scale)), log = TRUE))

    if (i %in% iter_keep) {
      ll_all_keep[, which(iter_keep == i)] <- dbinom(y, 1, 1 / (1 + exp(-pred_mean_model_scale)), log = TRUE)
    }
  }

  out <- list(
    w = w_keep,
    regcoef = regcoef_keep[, (nRE + 1):n_regcoef],
    loglik = ll_sum_keep,
    ll_all_keep = ll_all_keep
  )

  if (REmodel) {
    out$RE <- regcoef_keep[, 1:nRE]
    out$REsd <- 1 / sqrt(REprec_keep)
  }

  return(out)
}

# refractored bldim1 so that it calls gaussian or logistic fit
# had to do this in order to implement parallel chain
# use posterior package
# remove the draws to save memory. Don't see any use of the burn in. If the chains
# havent mixed, we will see it in the MCMC diagnostic anyway
# fixed bdlim1_logistic dbinom bug
