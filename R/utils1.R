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

bdlim1_gaussian_partial <- function(y, nits, design, REmodel, REprec, nRE, n_regcoef) {
  # Initialize variables
  n <- length(y)
  sigma <- stats::sd(y)
  sigma_keep <- numeric(nits)
  pred_mean_model_scale <- numeric(n)

  # Declare V outside the loop
  V <- matrix(0, nrow = ncol(design), ncol = ncol(design))

  # MCMC loop
  for (i in 1:nits) {
    # Update regression coefficients
    V <- t(design) %*% design / (sigma^2)

    # Calculate diag_v inside the loop
    if (REmodel) {
      diag_v <- c(rep(REprec, nRE), rep(1 / 100, n_regcoef - nRE))
    } else {
      diag_v <- rep(1 / 100, n_regcoef)
    }

    # Update diagonal of V
    diag(V) <- diag(V) + diag_v

    # Store sigma for this iteration
    sigma_keep[i] <- sigma

    # Other calculations will go here
  }

  # Prepare and return results
  out <- list(
    n = n,
    sigma = sigma,
    sigma_keep = sigma_keep,
    pred_mean_model_scale = pred_mean_model_scale,
    V = V
  )

  return(out)
}
