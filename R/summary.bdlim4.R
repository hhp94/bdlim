#' Summarize [bdlim4()] Object
#'
#' Summarizes a fitted `bdlim4` object.
#'
#' @param object An object fitted with [bdlim4()].
#' @param model A character string specifying which model to summarize. If `NULL`, the best fitting model is selected based on model probability.
#' @param exponentiate A logical value indicating whether to exponentiate the results if the logit link is used. Default is `FALSE`.
#' @param probs A numeric vector of probabilities to compute the quantiles. Default is `c(0.025, 0.975)`.
#' @param ... Additional arguments passed to `posterior::summarize_draws`.
#'
#' @return A `summary.bdlim4` object.
#' @export
#' @example inst/examples/summary_example.R
summary.bdlim4 <- function(object, model = NULL, exponentiate = FALSE, probs = c(0.025, 0.975), ...) {
  out <- list(
    modelcompare = modelcompare(object),
    WAIC = object$WAIC,
    call = object$call
  )

  # Find the best fitting model if not specified
  if (is.null(model)) {
    model <- names(which.max(out$modelcompare))
  }

  # Limit to selected model. Passed to summary.bdlim1
  sbdlim1 <- summary.bdlim1(object[[paste0("fit_", model)]], exponentiate = exponentiate, probs = probs, ...)
  sbdlim1$WAIC <- NULL
  sbdlim1$call <- NULL

  out <- c(out, sbdlim1)
  class(out) <- "summary.bdlim4"
  return(out)
}

#' Summarize [bdlim1()] Object
#'
#' @inheritParams summary.bdlim4
#'
#' @return An `summary.bdlim1` object.
#' @export
summary.bdlim1 <- function(object, exponentiate = FALSE, probs = c(0.025, 0.975), ...) {
  stopifnot(is.logical(exponentiate) && length(exponentiate) == 1)

  out <- list(WAIC = object$WAIC$WAIC, call = object$call)

  iter_keep <- seq(object$nburn + 1, object$nits, by = object$nthin)

  dots <- list(...)

  if (length(dots) == 0) {
    quantile3 <- function(.x) {
      posterior::quantile2(x = .x, probs = probs)
    }
    default_args <- list(
      "mean", "median", "sd", "quantile3", "rhat", "ess_bulk", "ess_tail"
    )
  } else {
    default_args <- NULL
  }

  object$draws <- posterior::subset_draws(object$draws, iteration = iter_keep)
  object$draws <- posterior::as_draws_array(object$draws)

  if (object$family == "binomial" && exponentiate) {
    for (i in c(object$ce, object$dlfun, object$regcoef)) {
      object$draws[, , i] <- exp(object$draws[, , i])
    }
  }
  # `cumulative` table
  out$cumulative <- posterior::summarize_draws(
    object$draws[, , object$variable$ce], default_args, ...
  )

  # Extract group for cumulative effects
  # Regex explanation:
  # ^ce_ matches the start of the string followed by "ce_"
  # (.*) captures everything after "ce_" into a group
  # $ ensures we've reached the end of the string
  # \\1 in the replacement refers to the captured group.
  # This same pattern is used for `dlfun`.
  out$cumulative$group <- sub("^ce_(.*)$", "\\1", out$cumulative$variable)

  # Move group column to the front for cumulative effects
  out$cumulative <- out$cumulative[, c("group", setdiff(names(out$cumulative), "group"))]

  # Sort cumulative by group
  out$cumulative <- out$cumulative[order(out$cumulative$group), ]

  # `dlfun` table
  out$dlfun <- posterior::summarize_draws(
    object$draws[, , object$variable$dlfun], default_args, ...
  )

  # Extract time and group for distributed lag functions
  out$dlfun$time <- as.numeric(sub(".*_(\\d+)$", "\\1", out$dlfun$variable))
  out$dlfun$group <- sub("^Ew_(.*)_\\d+$", "\\1", out$dlfun$variable)

  # Move group and time columns to the front for distributed lag functions
  out$dlfun <- out$dlfun[, c("group", "time", setdiff(names(out$dlfun), c("group", "time")))]

  # Sort `dlfun` by group and time
  out$dlfun <- out$dlfun[order(out$dlfun$group, out$dlfun$time), ]

  # `regcoef` table
  out$regcoef <- posterior::summarize_draws(
    object$draws[, , object$variable$regcoef], default_args, ...
  )

  # Add other variables
  for (i in c(object$variable$sigma, object$variable$RE, object$variable$REsd)) {
    if (!is.null(i)) {
      out[[i]] <- posterior::summarize_draws(object$draws[, , i])
    }
  }

  if (object$REmodel) {
    out$nRElevels <- length(object$variable$RE)
  }

  out <- c(out, object[c("names_groups", "n", "family", "chains", "model", "variable", "MCMC_check")])
  out$exponentiate <- exponentiate

  class(out) <- "summary.bdlim1"
  return(out)
}
