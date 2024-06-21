bdlim4_test <- function(
    y,
    exposure,
    covars,
    group,
    inter.model = c("all", "bw", "b", "w", "n"),
    id = NULL,
    df,
    nits,
    nburn = round(nits / 2),
    nthin = 1,
    future_args = list(future.seed = TRUE),
    family = c("gaussian", "binomial")
  ) {
  family <- match.arg(family)
  family <- toupper(family)
  inter.model <- match.arg(inter.model, several.ok = TRUE)
  if ("all" %in% inter.model) {
    inter.model <- c("bw", "b", "w", "n")
  }

  # Validate input
  validate_bdlim(
    y = y, exposure = exposure, covars = covars, group = group, id = id, df = df,
    nits = nits, nburn = nburn, nthin = nthin, family = family
  )

  # Get the correct bdlim function
  bdlim1_fam <- switch(family, GAUSSIAN = bdlim1, BINOMIAL = bdlim1_logistic)

  # Validate `future_args`
  stopifnot("nits has to be larger than nburn" = nits > nburn)
  stopifnot(
    "future_args should be a named list of args. See ?future.apply::future_lapply" =
      is.list(future_args) & !is.null(names(future_args))
  )

  if (!"future.seed" %in% names(future_args)) {
    future_args <- c(future_args, list(future.seed = TRUE))
  }

  if (!future_args$future.seed) {
    warning("Provide future.seed = TRUE to future_args to prevent unexpected behavior when running in parallel.")
  }

  # define the parameters of the 4 models
  model_params <- list(
    bw = list(model = "bw", w_free = TRUE, b_free = TRUE),
    b = list(model = "b", w_free = FALSE, b_free = TRUE),
    w = list(model = "w", w_free = TRUE, b_free = FALSE),
    n = list(model = "n", w_free = FALSE, b_free = FALSE)
  )

  model_params <- model_params[inter.model]
  fit_names <- paste("fit", inter.model, sep = "_")

  if (future::nbrOfWorkers() == 1) {
    message("fitting sequentially\n")
    f <- lapply
    future_args <- NULL
  } else {
    f <- future.apply::future_lapply
    message("fitting in parallel\n")
  }

  # fit each model
  out <- do.call(
    f,
    args = c(list(
      X = model_params,
      FUN = function(m) {
        message(paste0("fitting ", m$model, "\n"))
        bdlim1_fam(
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
          nthin = nthin
        )
      }
    ), future_args)
  )

  names(out) <- fit_names
  message("postprocessing")

  # likelihood comparison
  out$loglik <- as.data.frame(lapply(fit_names, function(x) {
    out[[x]]$loglik
  }), col.names = inter.model)

  # WAIC comparison
  out$WAIC <- as.data.frame(lapply(fit_names, function(x) {
    out[[x]]$WAIC$WAIC
  }), col.names = inter.model)

  # compile results
  out$nits <- nits
  out$nburn <- nburn
  out$nthin <- nthin
  out$call <- match.call()

  class(out) <- "bdlim4"
  return(out)
}

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
    family = c("GAUSSIAN", "BINOMIAL")) {
  # Validate y
  family <- match.arg(family)

  if (family == "GAUSSIAN") {
    if (!is.numeric(y)) {
      stop("For gaussian family, y should be numeric.")
    }
  }

  if (family == "BINOMIAL") {
    unique_y <- unique(y)
    error_message <- "For binomial family, y should be a numeric vector of 0s and 1s."

    if (!is.numeric(y)) {
      stop(error_message)
    }

    if (length(unique_y) != 2) {
      stop(error_message)
    }

    if (!all(sort(unique_y) == c(0, 1))) {
      stop(error_message)
    }
  }

  # Validate exposure and covars
  if (!(is.data.frame(exposure) || is.matrix(exposure))) {
    stop(paste("`exposure` should be a data.frame or matrix, not", paste(class(exposure), collapse = ", ")))
  }

  if (!(is.data.frame(covars) || is.matrix(covars))) {
    stop(paste("`covars` should be a data.frame or matrix, not", paste(class(covars), collapse = ", ")))
  }

  if (is.null(colnames(covars)) || is.null(colnames(exposure))) {
    warning("`covars` and `exposure` should be named.")
  }

  # Validate group
  if (!is.factor(group)) {
    stop("`group` must be a factor variable.")
  }

  # Validate id
  if (!is.null(id)) {
    if (!is.factor(id)) {
      stop("`id` must be a factor variable.")
    }
    n_id <- length(unique(droplevels(id)))
    if (n_id == length(y)) {
      warning("`id` are uniquely identifying a row. This is unexpected.")
    }
  }

  # Validate MCMC params
  params <- list(df = df, nits = nits, nburn = nburn, nthin = nthin)
  for (param_name in names(params)) {
    param_value <- params[[param_name]]
    if (!is.numeric(param_value) || length(param_value) != 1 || as.integer(param_value) != param_value || param_value <= 0) {
      stop(paste(param_name, "should be a positive integer."))
    }
  }

  if (nits <= nburn) {
    stop("`nits` has to be larger than `nburn`.")
  }

  if (nthin >= (nits - nburn)) {
    stop("`nthin` cannot be larger or equal to the kept chain length (nits - nburn).")
  }

  # Calculate lengths and number of rows
  ny <- length(y)
  ne <- nrow(exposure)
  nc <- nrow(covars)
  ng <- length(group)
  ni <- if (!is.null(id)) length(id) else NULL

  # Check for consistency in the number of rows/lengths between inputs
  consistent_lengths <- unique(c(ny, ne, nc, ng, ni))

  # Ensure all lengths/rows are consistent
  if (length(consistent_lengths) != 1) {
    stop("Inconsistent number of rows between inputs found.")
  }
}
