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
    result <- rbind(result, data.frame(
      name = arg_name,
      class = arg_class,
      length = arg_length,
      dimensions = arg_dim,
      stringsAsFactors = FALSE
    ))
  }

  return(result)
}

#' @keywords internal
#' @noRd
bdlim1_gaussian_partial <- function(
    y, nits, design, nRE, REmodel, w_group_ids
  ) {

  # starting values specific for `bdlim1_gaussian`
  n <- length(y)
  sigma <- stats::sd(y)
  sigma_keep <- rep(NA, nits)
  pred_mean_model_scale <- NA
  n_regcoef <- ncol(design)
  RElocation <- 1:nRE
  REprec <- 0.01
  n_weight_groups <- length(w_group_ids)

  for (i in 1:nits) {
    # update regression coefficients
    V <- t(design) %*% design / (sigma ^ 2)
    diag(V) <- diag(V) + c(rep(REprec, nRE), rep(0.01, n_regcoef - nRE))
    V <- chol2inv(chol(V))
    m <- drop(V %*% (t(design) %*% y)) / (sigma ^ 2)
    regcoef <- drop(m + t(chol(V)) %*% stats::rnorm(n_regcoef))

    # update sigma for Gaussian model
    sigma <- 1 / sqrt(stats::rgamma(1, .5 + n / 2, .5 + sum((
      y - design %*% regcoef
    ) ^ 2) / 2))

    # update random effect variance if a RE model
    if (REmodel) {
      REprec <- stats::rgamma(1, .5 + nRE / 2, .5 + sum(regcoef[RElocation] ^ 2) / 2)
    }
  }

  for (j in 1:n_weight_groups) {
    # log likelihood to start update of theta/w
    ll <- sum(stats::dnorm(y[w_group_ids[[j]]], design[w_group_ids[[j]], ] %*% regcoef, sigma, log = TRUE))
  }

  out <- list(sigma = sigma, regcoef = regcoef, m = m, V = V, ll = ll)

  if (REmodel) {
    out$REprec <- REprec
  }
  return(out)
}
