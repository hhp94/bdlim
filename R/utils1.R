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
bdlim1_gaussian_partial <- function(y,
                                    nits,
                                    design,
                                    nRE,
                                    REmodel,
                                    w_group_ids,
                                    basis,
                                    theta,
                                    Edesign,
                                    exposure,
                                    w,
                                    w_keep,
                                    regcoef_keep,
                                    ll_sum_keep,
                                    ll_all_keep,
                                    iter_keep) {
  # starting values specific for `bdlim1_gaussian`
  n <- length(y)
  sigma <- stats::sd(y)
  sigma_keep <- rep(NA, nits)
  pred_mean_model_scale <- NA
  n_regcoef <- ncol(design)
  RElocation <- 1:nRE
  REprec <- 0.01
  n_weight_groups <- length(w_group_ids)
  df <- ncol(basis)

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
    }

    # save values
    w_keep[i, ] <- c(t(w))
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

  out <- list(
    sigma = sigma,
    regcoef = regcoef,
    m = m,
    ll = ll,
    threshold = threshold,
    theta_prop = theta_prop,
    w = w,
    design = design,
    eta = eta,
    theta = theta,
    w_keep = w_keep,
    regcoef_keep = regcoef_keep,
    sigma_keep = sigma_keep
  )

  if (REmodel) {
    out$REprec <- REprec
  }

  return(out)
}
