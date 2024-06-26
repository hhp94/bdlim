test_that("validation works", {
  d <- sbd_bdlim[1:50, ]
  d$id <- factor(1:50)
  d$id_error <- 1:50
  d$y_gaussian_error <- as.character(d$bwgaz)
  d$y_binom <- rbinom(nrow(d), 1, 0.5)
  d$y_binom_error <- factor(d$y_binom)

  expect_no_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = d[, paste0("pm25_", 1:37)],
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 100,
      nburn = 50,
      nthin = 1,
      nchains = 1,
      family = "GAUSSIAN"
    )
  )

  expect_no_error(
    validate_bdlim(
      y = d$y_binom,
      exposure = d[, paste0("pm25_", 1:37)],
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 100,
      nburn = 50,
      nthin = 1,
      nchains = 1,
      family = "BINOMIAL"
    )
  )

  # manually change and test to avoid copying pasting too many scenarios
  # validate_bdlim(
  #   y = d$y_binom,
  #   exposure = d[, paste0("pm25_", 1:37)],
  #   covars = d[, c("MomPriorBMI"), drop = FALSE],
  #   group = d$ChildSex,
  #   id = NULL,
  #   df = 5,
  #   nits = 100,
  #   nburn = 9,
  #   nthin = 1,
  #   nchains = 1,
  #   family = "BINOMIAL"
  # )
})

test_that("bdlim4 didn't change from old results", {
  set.seed(1234)
  old_fit_gaussian <- bdlim4_before(
    y = sbd_bdlim$bwgaz,
    exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
    covars = sbd_bdlim[, c("MomPriorBMI"), drop = FALSE],
    group = sbd_bdlim$ChildSex,
    df = 5,
    nits = 100
  )

  set.seed(1234)
  new_fit_gaussian <- bdlim4(
    y = sbd_bdlim$bwgaz,
    exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
    covars = sbd_bdlim[, c("MomPriorBMI"), drop = FALSE],
    group = sbd_bdlim$ChildSex,
    df = 5,
    nits = 100
  )

  modify_fits <- function(fit) {
    fit$call <- NULL
    for (j in paste("fit", c("bw", "b", "w", "n"), sep = "_")) {
      if (!is.null(fit[[j]]) && !is.null(fit[[j]][["call"]])) {
        fit[[j]][["call"]] <- NULL
      }
    }
    return(fit)
  }

  old_fit_gaussian <- modify_fits(old_fit_gaussian)
  new_fit_gaussian <- modify_fits(new_fit_gaussian)

  expect_identical(old_fit_gaussian, new_fit_gaussian)
})

test_that("bdlim4 model selection works", {
  # Only fitting model b, and w.
  set.seed(1234)
  new_fit_gaussian <- bdlim4(
    y = sbd_bdlim$bwgaz,
    exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
    covars = sbd_bdlim[, c("MomPriorBMI"), drop = FALSE],
    model = c("b", "w"),
    group = sbd_bdlim$ChildSex,
    df = 5,
    nits = 100
  )
  expect_true(all(c("fit_b", "fit_w") %in% names(new_fit_gaussian)))
  expect_true(all(!c("fit_bw", "fit_n") %in% names(new_fit_gaussian)))

  sfit <- summary(new_fit_gaussian)
  sfit_b <- summary(new_fit_gaussian, model = "b")

  expect_identical(sfit, sfit_b)
  # bw is not fitted, so summary model = "bw" won't work
  expect_error(summary(new_fit_gaussian, model = "bw"))

  # Fit only 1 model
  set.seed(1234)
  fit_bw <- bdlim4(
    y = sbd_bdlim$bwgaz,
    exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
    covars = sbd_bdlim[, c("MomPriorBMI"), drop = FALSE],
    model = c("bw"),
    group = sbd_bdlim$ChildSex,
    df = 5,
    nits = 100
  )

  expect_no_error(summary(fit_bw))
  expect_no_error(plot(summary(fit_bw)))
})
