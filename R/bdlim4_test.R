bdlim4_test <- function(y, exposure, covars, group, id = NULL,
                        df, nits, nburn = round(nits / 2), nthin = 1, future_args = list(future.seed = TRUE),
                        family = c("gaussian", "binomial")) {
  # make sure group is a factor variable
  # this is redundant with bdlim1 but makes the error message clearer to place here also
  family <- match.arg(family)
  family <- toupper(family)
  bdlim1_fam <- switch(family,
    GAUSSIAN = bdlim1,
    BINOMIAL = bdlim1_logistic
  )

  if (!is.factor(group)) {
    stop("group must be a factor variable.")
  }
  # make sure id is factor
  if (!is.null(id)) {
    if (!is.factor(id)) {
      stop("id must be a factor variable.")
    }
  }

  if (!(is.data.frame(exposure) | is.matrix(exposure))) {
    stop(
      paste("exposure should be a data.frame or matrix, not", paste(class(exposure), collapse = ", "))
    )
  }

  if (!(is.data.frame(covars) | is.matrix(covars))) {
    stop(
      paste("covars should be a data.frame or matrix, not", paste(class(covars), collapse = ", "))
    )
  }

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
  fit_names <- paste("fit", names(model_params), sep = "_")

  if(future::nbrOfWorkers() == 1) {
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
  out$loglik <- data.frame(
    bw = out$fit_bw$loglik,
    b = out$fit_b$loglik,
    w = out$fit_w$loglik,
    n = out$fit_n$loglik
  )

  out$WAIC <- data.frame(
    bw = out$fit_bw$WAIC$WAIC,
    b = out$fit_b$WAIC$WAIC,
    w = out$fit_w$WAIC$WAIC,
    n = out$fit_n$WAIC$WAIC
  )

  # compile results
  out$nits <- nits
  out$nburn <- nburn
  out$nthin <- nthin
  out$call <- match.call()

  class(out) <- "bdlim4"
  return(out)
}
