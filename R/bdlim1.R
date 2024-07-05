#' Fit the BDLIM Model with One Pattern of Modification
#'
#' Warning: Use [bdlim4()] with the `model` argument to fit the desired pattern instead. This function does not validate the inputs.
#'
#' @inheritParams bdlim4
#' @param w_free Logical indicating if the weight functions are shared by all groups (FALSE) or group-specific (TRUE).
#' @param b_free Logical indicating if the effect sizes are shared by all groups (FALSE) or group-specific (TRUE).
#'
#' @return A `bdlim1` object.
#' @export
bdlim1 <- function(y, exposure, covars, group, id, w_free, b_free, df, nits, nburn, nthin, chains, family, loglik_all, cpp) {
  # switch between family
  if (cpp) {
    family <- paste0(family, "_", "cpp")
  }
  bdlim1_fit <- switch(family,
    gaussian = bdlim1_gaussian,
    binomial = bdlim1_logistic,
    gaussian_cpp = bdlim1_gaussian_cpp,
    stop("Unsupported Family")
  )

  # bind all data into one data.frame.
  # Because we no longer allow NA, we don't need to add exposure to `alldata`.
  # group has to be the first factor, otherwise stats::model.matrix won't create
  # a term for each group
  alldata <- data.frame(y = y, group = group)
  if (!is.null(covars)) {
    alldata <- droplevels(cbind(alldata, covars))
  }
  # add random effect matrix here
  if (!is.null(id)) {
    id <- droplevels(id)
    RE <- stats::model.matrix(~ id - 1)
    colnames(RE) <- paste0(".id", "_", seq_len(ncol(RE)))
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
  n_groups <- length(levels(alldata$group))
  names_groups <- make.names(levels(alldata$group), unique = TRUE, allow_ = FALSE)
  n_times <- ncol(exposure)
  # design matrix for covariates and main effects of group
  design <- stats::model.matrix(y ~ . - 1, data = alldata)

  # Calculate group_loc
  group_loc <- (nRE + 1):(nRE + n_groups)

  # basis for weights
  basisObj <- makebasis(exposure, df = df)
  basis <- basisObj$u

  # preliminary weighted exposures
  # make flat for all groups
  theta <- stats::lm(rep(1 / sqrt(n_times), n_times) ~ basis - 1)$coef
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
    Edesign <- design[, group_loc]
    colnames(Edesign) <- paste0(".E", "_", names_groups)
  } else {
    Edesign <- matrix(1, n, 1)
    colnames(Edesign) <- ".E"
  }

  # add weighted exposures to design matrix.
  design <- cbind(design, Edesign * drop(E))
  n_regcoef <- ncol(design)

  # init w_keep as a 3D array of n_weight_groups * n_times * nits
  w_keep <- array(NA, dim = c(n_weight_groups, n_times, nits))

  # index for groups for weights
  # identifies which rows are in which weight groups
  w_group_ids <- list()

  if (w_free) {
    for (j in seq_along(group_loc)) {
      w_group_ids[[j]] <- which(design[, group_loc[j]] == 1)
    }
    dimnames(w_keep) <- list(paste0(".W", "_", names_groups), seq_len(n_times), seq_len(nits))
  } else {
    w_group_ids[[1]] <- 1:n
    dimnames(w_keep) <- list(".W", seq_len(n_times), seq_len(nits))
  }

  ## RE precision
  REprec <- 0.01

  ## place to store results
  regcoef_keep <- matrix(NA, nits, n_regcoef)
  ll_sum_keep <- REprec_keep <- rep(NA, nits)

  ## iterations to be kept
  ## this is only used for waic
  iter_keep <- seq(nburn + 1, nits, by = nthin)
  ll_all_keep <- matrix(NA, n, length(iter_keep))

  # format
  colnames(regcoef_keep) <- colnames(design)
  colnames(regcoef_keep)[group_loc] <- paste0("intercept", names_groups)

  # if chains == 1, switch to lapply to 100% reproduce 0.4 version fit
  if (chains == 1) {
    chain_fit <- lapply
  } else {
    chain_fit <- future.apply::future_lapply
  }

  # Separate arguments into fit_args and r_args
  fit_args <- list(
    y = y,
    w = w,
    nits = nits,
    design = design,
    nRE = nRE,
    REmodel = REmodel,
    w_group_ids = w_group_ids,
    theta = theta,
    basis = basis,
    Edesign = Edesign,
    exposure = exposure
  )

  r_args <- list(
    REprec = REprec,
    n_regcoef = n_regcoef,
    RElocation = RElocation,
    n_weight_groups = n_weight_groups,
    df = df,
    w_keep = w_keep,
    regcoef_keep = regcoef_keep,
    REprec_keep = REprec_keep,
    ll_sum_keep = ll_sum_keep,
    ll_all_keep = ll_all_keep,
    names_groups = names_groups,
    n_times = n_times,
    b_free = b_free,
    w_free = w_free,
    n_groups = n_groups,
    iter_keep = iter_keep
  )

  if (!cpp) {
    fit_args <- c(fit_args, r_args)
  }

  out <- chain_fit(
    seq_len(chains),
    function(x, future.seed = TRUE) {
      # Use do.call to call bdlim1_fit with fit_args
      f <- do.call(bdlim1_fit, fit_args)

      if (cpp) {
        return(
          process_cpp(
            f,
            w_keep = r_args$w_keep,
            regcoef_keep = r_args$regcoef_keep,
            iter_keep = r_args$iter_keep,
            names_groups = r_args$names_groups,
            b_free = r_args$b_free,
            w_free = r_args$w_free,
            nRE = fit_args$nRE,
            n_regcoef = r_args$n_regcoef
          )
        )
      }

      return(f)
    },
    future.seed = TRUE
  )

  out <- process_chains(out)

  out <- c(
    out,
    list(
      n = n,
      nits = nits,
      nburn = nburn,
      nthin = nthin,
      REmodel = REmodel,
      family = family,
      names_groups = names_groups,
      model = get_model_name(w_free = w_free, b_free = b_free),
      n_times = n_times,
      REmodel = REmodel,
      loglik = posterior::merge_chains(
        posterior::subset_draws(out$draws, variable = "loglik")
      ),
      WAIC = LaplacesDemon::WAIC(out$ll_all_keep),
      basisObj = basisObj,
      call = match.call()
    )
  )

  # Collect all variable names for easier handling
  variable <- dimnames(out$draws)$variable
  out$variable <- list(
    w = grep(".W_", variable, value = TRUE),
    regcoef = colnames(regcoef_keep[, (nRE + 1):n_regcoef]),
    dlfun = grep(".dl_", variable, value = TRUE),
    ce = grep(".ce_", variable, value = TRUE),
    Edesign = colnames(Edesign),
    sigma = if (family == "gaussian") "sigma" else NULL,
    RE = if (REmodel) colnames(regcoef_keep[, 1:nRE]) else NULL,
    REsd = if (REmodel) "REsd" else NULL
  )

  # Summarize MCMC Convergence
  out$MCMC_check <- posterior::summarize_draws(out$draws, "rhat", "ess_bulk", "ess_tail")

  if (!loglik_all) {
    out$ll_all_keep <- NULL
  }

  class(out) <- "bdlim1"

  return(out)
}

#' Process the CPP output to harmonize with the R output
#'
#' @keywords internal
#'
#' @noRd
process_cpp <- function(out, w_keep, regcoef_keep, iter_keep, names_groups, b_free, w_free, nRE, n_regcoef) {
  dimnames(out$w_keep) <- dimnames(w_keep)
  colnames(out$regcoef_keep) <- colnames(regcoef_keep)
  out$loglik <- colSums(out$ll_all_keep)
  out$ll_all_keep <- out$ll_all_keep[, iter_keep, drop = F]
  out$w_dlfun_ce <- process_w_dlfun_ce(
    w_keep = out$w_keep,
    regcoef_keep = out$regcoef_keep,
    names_groups = names_groups,
    b_free = b_free,
    w_free = w_free
  )

  out$sigma <- drop(out$sigma_keep)
  out$sigma_keep <- NULL
  out$w_keep <- NULL
  out <- c(
    out,
    asplit(out$regcoef_keep, 2),
    asplit(out$w_dlfun_ce, 2)
  )
  out$w_dlfun_ce <- NULL
  if (!is.null(out$REprec_keep)) {
    out$REsd <- 1 / drop(sqrt(out$REprec_keep))
    out$REprec_keep <- NULL
  }
  out$regcoef_keep <- NULL
  return(out)
}

#' Combine Multiple Chains
#'
#' Combine MCMC Chains into a `posterior` object. The log likelihood matrix is an N*S matrix
#' where N is the number of rows and S is the number of draws.
#'
#' @param out A `bdlim1_gaussian` or `bdlim1_logistic` fit.
#' @keywords internal
#' @noRd
process_chains <- function(out) {
  # Get the names of the elements in each chain excluding `ll_all_keep`
  param <- setdiff(names(out[[1]]), "ll_all_keep")

  # Convert each chain to a consistent format for other MCMC packages
  draws <- posterior::as_draws_list(lapply(out, function(x) x[param]))
  draws <- posterior::as_draws_array(draws)

  # Combine the log likelihood draws matrix for LOO and WAIC calculations
  ll_all_keep <- do.call(cbind, lapply(out, function(x) x[["ll_all_keep"]]))

  return(list(draws = draws, ll_all_keep = ll_all_keep))
}

#' Flatten w_keep and Calculate dlfun and ce
#'
#' @keywords internal
#' @noRd
process_w_dlfun_ce <- function(w_keep, regcoef_keep, names_groups, b_free, w_free) {
  w <- dlfun <- ce <- list()
  w_names <- dimnames(w_keep)[[2]]
  if (!b_free && !w_free) {
    # There's only one row of w_keep and one column E of regcoef_keep.
    # Times each time point in w_keep with column E in regcoef
    w[[1]] <- t(w_keep[1, , ])
    colnames(w[[1]]) <- paste0(".W", "_", w_names)
    dlfun[[1]] <- t(w_keep[1, , ]) * drop(regcoef_keep[, ".E"])
    colnames(dlfun[[1]]) <- paste0(".dl", "_", w_names)
    ce[[1]] <- rowSums(dlfun[[1]])
    names(ce) <- ".ce_"
  } else {
    for (group in names_groups) {
      # Determine the correct w_keep row and regcoef column
      W_row <- if (w_free) paste0(".W", "_", group) else ".W"
      E_col <- if (b_free) paste0(".E", "_", group) else ".E"

      for (i in W_row) {
        # will only have 1 member if !w_free
        w[[i]] <- t(w_keep[i, , ])
        colnames(w[[i]]) <- paste0(i, "_", w_names)
      }

      # Calculate dlfun for this group using matrix multiplication
      dlfun[[group]] <- t(w_keep[W_row, , ]) * drop(regcoef_keep[, E_col])

      # Set column names
      colnames(dlfun[[group]]) <- paste0(".dl", "_", group, "_", w_names)

      # Calculate ce for this group
      ce[[group]] <- rowSums(dlfun[[group]])
    }
    names(ce) <- paste0(".ce", "_", names_groups)
  }

  return(cbind(do.call(cbind, w), do.call(cbind, dlfun), do.call(cbind, ce)))
}

#' @keywords internal
#' @noRd
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
    w_free,
    n_groups,
    iter_keep) {
  # starting values specific for `bdlim1_gaussian`
  n <- length(y)
  sigma <- stats::sd(y)
  sigma_keep <- rep(NA, nits)
  pred_mean_model_scale <- NA

  for (i in 1:nits) {
    # update regression coefficients
    V <- t(design) %*% design / (sigma^2)
    diag(V) <- diag(V) + c(rep(REprec, nRE), rep(1 / 100, n_regcoef - nRE))
    V <- chol2inv(chol(V))
    m <- drop(V %*% (t(design) %*% y)) / (sigma^2)
    regcoef <- drop(m + t(chol(V)) %*% stats::rnorm(n_regcoef))

    # update sigma for Gaussian model
    sigma <- 1 / sqrt(stats::rgamma(1, .5 + n / 2, .5 + sum((y - design %*% regcoef)^2) / 2))

    # update random effect variance if a RE model
    if (REmodel) {
      REprec <- stats::rgamma(1, .5 + nRE / 2, .5 + sum(regcoef[RElocation]^2) / 2)
    }

    for (j in 1:n_weight_groups) {
      # log likelihood to start update of theta/w
      ll <- sum(stats::dnorm(y[w_group_ids[[j]]], design[w_group_ids[[j]], ] %*% regcoef, sigma, log = TRUE))
      threshold <- ll + log(stats::runif(1))
      ll <- threshold - 1 # allows always to start loop

      # vector for ellipse
      nu <- matrix(stats::rnorm(df), 1, df)
      eta_max <- eta <- stats::runif(1, 0, 2 * pi)
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
        ll <- sum(stats::dnorm(y[w_group_ids[[j]]], design[w_group_ids[[j]], ] %*% regcoef, sigma, log = TRUE))
        # adjust eta in case repeat
        if (eta < 0) {
          eta_min <- eta
        } else {
          eta_max <- eta
        }
        eta <- stats::runif(1, eta_min, eta_max)
      }

      # update theta (w and design are already updated)
      theta[j, ] <- theta_prop

      # keep w draws
      w_keep[j, , i] <- w[j, ]
    }

    # save values
    regcoef_keep[i, ] <- regcoef
    sigma_keep[i] <- sigma
    if (REmodel) {
      REprec_keep[i] <- REprec
    }
    pred_mean_model_scale <- design %*% regcoef
    # calculate log-likelihood once
    ll_all <- stats::dnorm(y, pred_mean_model_scale, sigma, log = TRUE)
    ll_sum_keep[i] <- sum(ll_all)
    if (i %in% iter_keep) {
      ll_all_keep[, which(iter_keep == i)] <- ll_all
    }
  }

  # Flatten w_keep after draw from group * times * iters to iters * group_times
  w_dlfun_ce <- process_w_dlfun_ce(
    w_keep = w_keep,
    regcoef_keep = regcoef_keep,
    names_groups = names_groups,
    b_free = b_free,
    w_free = w_free
  )

  out <- c(
    asplit(regcoef_keep, 2),
    list(sigma = sigma_keep),
    asplit(w_dlfun_ce, 2),
    list(loglik = ll_sum_keep),
    list(ll_all_keep = ll_all_keep)
  )

  if (REmodel) {
    out <- c(
      out,
      list(REsd = 1 / sqrt(REprec_keep))
    )
  }

  return(out)
}

#' @keywords internal
#' @noRd
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
    w_free,
    n_groups,
    iter_keep) {
  n <- length(y)
  # set linear predictor to 0 for starting values
  pred_mean_model_scale <- rep(0, n)

  for (i in 1:nits) {
    # omega from PG augmentation
    w_pg <- BayesLogit::rpg(n, 1, pred_mean_model_scale)

    # update regression coefficients
    design_w <- t(scale(t(design), 1 / sqrt(w_pg), center = FALSE))
    V <- t(design_w) %*% design_w
    diag(V) <- diag(V) + c(rep(REprec, nRE), rep(1 / 100, n_regcoef - nRE))
    V <- chol2inv(chol(V))
    m <- drop(V %*% (t(design) %*% (y - .5)))
    regcoef <- drop(m + t(chol(V)) %*% stats::rnorm(n_regcoef))

    # update random effect variance if a RE model
    if (REmodel) {
      REprec <- stats::rgamma(1, .5 + nRE / 2, .5 + sum(regcoef[RElocation]^2) / 2)
    }

    for (j in 1:n_weight_groups) {
      # log likelihood to start update of theta/w
      ll <- sum(stats::dbinom(y[w_group_ids[[j]]], 1, 1 / (1 + exp(-design[w_group_ids[[j]], ] %*% regcoef)), log = TRUE))
      threshold <- ll + log(stats::runif(1))
      ll <- threshold - 1 # allows always to start loop

      # vector for ellipse
      nu <- matrix(stats::rnorm(df), 1, df)
      eta_max <- eta <- stats::runif(1, 0, 2 * pi)
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
        ll <- sum(stats::dbinom(y[w_group_ids[[j]]], 1, 1 / (1 + exp(-design[w_group_ids[[j]], ] %*% regcoef)), log = TRUE))
        # adjust eta in case repeat
        if (eta < 0) {
          eta_min <- eta
        } else {
          eta_max <- eta
        }
        eta <- stats::runif(1, eta_min, eta_max)
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
    # calculate log-likelihood once
    ll_all <- stats::dbinom(y, 1, 1 / (1 + exp(-pred_mean_model_scale)), log = TRUE)
    ll_sum_keep[i] <- sum(ll_all)

    if (i %in% iter_keep) {
      ll_all_keep[, which(iter_keep == i)] <- ll_all
    }
  }

    # Flatten w_keep after draw from group * times * iters to iters * group_times
  w_dlfun_ce <- process_w_dlfun_ce(
    w_keep = w_keep,
    regcoef_keep = regcoef_keep,
    names_groups = names_groups,
    b_free = b_free,
    w_free = w_free
  )

  out <- c(
    asplit(regcoef_keep, 2),
    asplit(w_dlfun_ce, 2),
    list(loglik = ll_sum_keep),
    list(ll_all_keep = ll_all_keep)
  )

  if (REmodel) {
    out <- c(
      out,
      list(REsd = 1 / sqrt(REprec_keep))
    )
  }

  return(out)
}
