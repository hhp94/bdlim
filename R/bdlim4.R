#' Fit the BDLIM Model with 4 Patterns of Modification
#'
#' @param y A vector of outcomes.
#' @param exposure A matrix or data.frame of exposures, with one row per individual and one column per time the exposure is observed.
#' @param covars A matrix or data.frame of covariates. Covariates should not include the grouping factor (see 'group'). Factor covariates may be included.
#' @param group A vector of group memberships. This should be a factor variable corresponding to each row of 'covars' or 'group'.
#' @param model A vector of model options including any of: "all", "bw", "b", "w", "n". Defaults to "all".
#' @param id An optional vector of individual IDs for cases with repeated measures or other groupings where a random intercept is needed. This must be a factor variable.
#' @param df Degrees of freedom for the weight functions passed to [splines::ns()].
#' @param nits Number of MCMC iterations.
#' @param nburn Number of MCMC iterations to be discarded as burn-in. Defaults to half the total number of iterations. Used for WAIC, but also passed to summary and plot functions.
#' @param nthin Thinning factor for MCMC. Used for WAIC, but also passed to summary and plot functions.
#' @param chains Number of parallel chains per model. Not yet implemented.
#' @param family Model family to use. Supported options are "gaussian" for a normal/gaussian linear model, and "binomial" for a logistic model.
#'
#' @return A list of results from each pattern of modification, including model comparison metrics.
#' @export
#'
#' @example inst/examples/bdlim_example.R
bdlim4 <- function(
    y,
    exposure,
    covars = NULL,
    group,
    model = c("all", "bw", "b", "w", "n"),
    id = NULL,
    df,
    nits,
    nburn = round(nits / 2),
    nthin = 1,
    chains = 1,
    family = c("gaussian", "binomial")) {
  # Validate family and model
  family <- match.arg(family)
  model <- unique(match.arg(model, several.ok = TRUE))
  if ("all" %in% model) {
    model <- c("bw", "b", "w", "n")
  }

  # Validate input
  validate_bdlim(
    y = y, exposure = exposure, covars = covars, group = group, id = id, df = df,
    nits = nits, nburn = nburn, nthin = nthin, chains = chains, family = family
  )

  # Define the parameters of the 4 models
  model_params <- list(
    bw = list(model = "bw", w_free = TRUE, b_free = TRUE),
    b = list(model = "b", w_free = FALSE, b_free = TRUE),
    w = list(model = "w", w_free = TRUE, b_free = FALSE),
    n = list(model = "n", w_free = FALSE, b_free = FALSE)
  )

  # Run requested models
  model_params <- model_params[model]
  fit_names <- paste("fit", model, sep = "_")

  # If only one worker is available, then use base `lapply` for sequential printing.
  if (future::nbrOfWorkers() == 1) {
    message("fitting sequentially\n")
    f <- lapply
    future_args <- NULL
  } else {
    message("fitting in parallel\n")
    f <- future.apply::future_lapply
    future_args <- list(future.seed = TRUE)
  }

  # Fit each model. For each `model_params` element, fit the `bdlim1` function of the
  # requested family.
  out <- do.call(
    f,
    args = c(list(
      X = model_params,
      FUN = function(m) {
        message(paste0("fitting ", m$model, "\n"))
        bdlim1(
          y = y,
          exposure = exposure,
          covars = covars,
          group = group,
          id = id,
          w_free = m$w_free,
          b_free = m$b_free,
          df = df,
          nits = nits,
          nburn = nburn,
          nthin = nthin,
          chains = chains,
          family = family
        )
      }
    ), future_args)
  )

  names(out) <- fit_names
  message("postprocessing")

  # likelihood comparison
  out$loglik <- as.data.frame(lapply(fit_names, function(x) {
    out[[x]]$loglik
  }), col.names = model)

  # WAIC comparison
  out$WAIC <- as.data.frame(lapply(fit_names, function(x) {
    out[[x]]$WAIC$WAIC
  }), col.names = model)

  # Compile results
  out$nits <- nits
  out$nburn <- nburn
  out$nthin <- nthin
  out$call <- match.call()

  class(out) <- "bdlim4"
  return(out)
}

#' Validate Common Inputs of [bdlim4()] and [bdlim1()]
#'
#' @inheritParams bdlim4
#'
#' @keywords internal
#' @noRd
#' @return 0
validate_bdlim <- function(
    y,
    exposure,
    covars,
    group,
    id,
    df,
    nits,
    nburn,
    nthin,
    chains,
    family = c("gaussian", "binomial")) {
  # Validate `family`
  family <- match.arg(family)

  # Validate `y` based on family
  if (family == "gaussian") {
    if (!is.numeric(y)) stop("For gaussian family, y should be numeric.")
  } else if (family == "binomial") {
    unique_y <- stats::na.omit(unique(y))
    if (!is.numeric(y) || length(unique_y) != 2 || !all(unique_y %in% c(0, 1))) {
      stop("For binomial family, y should be a numeric vector of 0s and 1s.")
    }
  } else {
    stop("Unexpected Family.")
  }

  # Validate `exposure`
  if (!is.data.frame(exposure) && !is.matrix(exposure)) {
    stop("`exposure` should be a data.frame or matrix, not ", paste(class(exposure), collapse = ", "))
  }
  if (is.matrix(exposure) && !is.numeric(exposure)) stop("`exposure` has to be numeric")
  if (is.data.frame(exposure) && any(vapply(exposure, function(x) !is.numeric(x), logical(1)))) {
    stop("`exposure` has to be numeric")
  }
  if (is.null(colnames(exposure))) warning("`exposure` should be named.")

  # Validate `covars`
  if (!is.null(covars)) {
    if (!is.data.frame(covars) && !is.matrix(covars)) {
      stop("`covars` should be a data.frame or matrix, not ", paste(class(covars), collapse = ", "))
    }
    if (is.matrix(covars) && is.character(covars)) {
      warning("`covars` is a matrix of characters (all covariates are factors). This is probably unintended.")
    }
    if (is.null(colnames(covars))) warning("`covars` should be named.")
  }

  # Validate `group`
  if (!is.factor(group)) stop("`group` must be a factor variable.")
  n_group <- length(unique(droplevels(group)))
  if (n_group == length(y)) stop("There cannot be as many `group` as observations.")
  if (n_group < 2) stop("`group` must have at least 2 levels.")
  if (n_group > 5) warning("A large number of `group` can make model fitting very slow.")
  if (!is.null(covars)) {
    # If `covars` is a matrix then we know it doesn't have factor variables. Here we check
    # to see if the grouping variable is included in `covars`
    if (is.data.frame(covars)) {
      factor_columns <- which(vapply(covars, is.factor, logical(1)))
      for (i in factor_columns) {
        if (all(as.character(covars[[i]]) == as.character(group))) {
          stop("The same variable as the grouping variable is detected in `covars`.")
        }
      }
    }
  }

  # Validate `id`
  if (!is.null(id)) {
    if (!is.factor(id)) stop("`id` must be a factor variable.")
    n_id <- length(unique(droplevels(id)))
    if (n_id == length(y)) stop("There cannot be as many `id` as observations.")
    if (n_id < 2) stop("`id` must have at least 2 levels.")
  }

  # Validate `df` and MCMC parameters
  validate_mcmc_param <- function(param_name, param_value) {
    if (!is.numeric(param_value) || length(param_value) != 1 || as.integer(param_value) != param_value || param_value < 0) {
      stop(param_name, " should be a non-negative integer.")
    }
  }
  params <- list(df = df, nits = nits, nburn = nburn, nthin = nthin, chains = chains)
  lapply(names(params), function(name) validate_mcmc_param(name, params[[name]]))

  if (nits <= 0 || chains <= 0) stop("`nits` and `chains` must be positive.")
  if (nits <= nburn) stop("`nits` has to be larger than `nburn`.")
  if (nthin >= (nits - nburn)) stop("`nthin` cannot be larger or equal to the kept chain length (nits - nburn).")

  # Validate lengths and number of rows
  ny <- length(y)
  ne <- nrow(exposure)
  nc <- if (!is.null(covars)) nrow(covars) else NULL
  ng <- length(group)
  ni <- if (!is.null(id)) length(id) else NULL

  # Check for consistency in the number of rows/lengths between inputs
  lengths <- c(ny, ne, nc, ng, ni)
  if (length(unique(lengths[!is.null(lengths)])) != 1) {
    stop("Inconsistent number of rows between inputs found.")
  }

  # Disallow NA
  if (any(c(anyNA(y), anyNA(exposure), anyNA(covars), anyNA(group), anyNA(id)))) {
    stop("NA found. Handling of NA is currently not supported.")
  }

  return(0)
}
