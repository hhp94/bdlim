debug_bdlim1_gaussian <- function() {
  # Get the formal arguments of bdlim1_gaussian
  args <- formals(bdlim1_gaussian)

  # Create an empty data frame to store the results
  result <- data.frame(
    name = character(),
    class = character(),
    length = integer(),
    dimensions = character(),
    stringsAsFactors = FALSE
  )

  # Loop through each argument
  for (arg_name in names(args)) {
    arg_value <- get(arg_name)

    # Get class
    arg_class <- class(arg_value)[1]

    # Get length
    arg_length <- length(arg_value)

    # Get dimensions
    if (is.null(dim(arg_value))) {
      arg_dim <- "NULL"
    } else {
      arg_dim <- paste(dim(arg_value), collapse = "x")
    }

    # Add to the result data frame
    result <- rbind(
      result,
      data.frame(
        name = arg_name,
        class = arg_class,
        length = arg_length,
        dimensions = arg_dim,
        stringsAsFactors = FALSE
      )
    )
  }

  return(result)
}

#' @keywords internal
#' @noRd
bdlim1_gaussian_partial <- function(
    y,
    design,
    nits,
    REmodel,
    nRE,
    REprec,
    w_group_ids,
    Edesign,
    basis,
    w,
    theta,
    exposure) {
  # starting values specific for `bdlim1_gaussian`
  n <- length(y)
  sigma <- stats::sd(y)
  sigma_keep <- rep(NA, nits)

  n_regcoef <- ncol(design)
  RElocation <- seq_len(nRE)
  n_weight_groups <- length(w_group_ids)
  df <- ncol(basis)
  n_times <- nrow(basis)
  n_groups <- ncol(Edesign)

  w_keep <- array(NA, dim = c(n_weight_groups, n_times, nits))
  regcoef_keep <- array(NA, dim = c(nits, n_regcoef))
  ll_all_keep <- array(NA, dim = c(n, nits))
  REprec_keep <- array(NA, dim = nits)

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

    # calculate log-likelihood once
    ll_all_keep[, i] <- stats::dnorm(y, design %*% regcoef, sigma, log = TRUE)
  }

  result <- list(
    w_keep = w_keep,
    regcoef_keep = regcoef_keep,
    sigma_keep = sigma_keep,
    ll_all_keep = ll_all_keep
  )

  if (REmodel) {
    result[["REprec_keep"]] <- REprec_keep
  }

  return(result)
}

#' @keywords internal
#' @noRd
bdlim1_logistic_partial <- function(
    y,
    design,
    nits,
    REmodel,
    nRE,
    REprec,
    w_group_ids,
    Edesign,
    basis,
    w,
    theta,
    exposure) {
  n <- length(y)
  # set linear predictor to 0 for starting values
  pred_mean_model_scale <- rep(0, n)

  n_regcoef <- ncol(design)
  RElocation <- seq_len(nRE)
  n_weight_groups <- length(w_group_ids)
  df <- ncol(basis)
  n_times <- nrow(basis)
  n_groups <- ncol(Edesign)

  w_keep <- array(NA, dim = c(n_weight_groups, n_times, nits))
  regcoef_keep <- array(NA, dim = c(nits, n_regcoef))
  ll_all_keep <- array(NA, dim = c(n, nits))
  REprec_keep <- array(NA, dim = nits)

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

      # keep w draws
      w_keep[j, , i] <- w[j, ]
    }

    # save values
    regcoef_keep[i, ] <- regcoef
    if (REmodel) {
      REprec_keep[i] <- REprec
    }

    # calculate log-likelihood once
    pred_mean_model_scale <- design %*% regcoef
    ll_all_keep[, i] <- stats::dbinom(y, 1, 1 / (1 + exp(-pred_mean_model_scale)), log = TRUE)
  }

  result <- list(
    w_keep = w_keep,
    regcoef_keep = regcoef_keep,
    ll_all_keep = ll_all_keep
  )

  if (REmodel) {
    result[["REprec_keep"]] <- REprec_keep
  }

  return(result)
}
