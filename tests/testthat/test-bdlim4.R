test_that("validation works for gaussian family", {
  d <- sbd_bdlim[1:50, ]
  d$id <- factor(1:50)
  d$y_binom <- rbinom(nrow(d), 1, 0.5)

  expect_no_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    )
  )
})

test_that("validation works for binomial family", {
  d <- sbd_bdlim[1:50, ]
  d$id <- factor(1:50)
  d$y_binom <- rbinom(nrow(d), 1, 0.5)

  expect_no_error(
    validate_bdlim(
      y = d$y_binom,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "binomial"
    )
  )
})

test_that("validation fails for incorrect y type in gaussian family", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = as.character(d$bwgaz),
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "For gaussian family, y should be numeric."
  )
})

test_that("validation fails for incorrect y type in binomial family", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "binomial"
    ),
    "For binomial family, y should be a numeric vector of 0s and 1s."
  )
})

test_that("validation fails for non-matrix exposure", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = d[, paste0("pm25_", 1:37)],
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "`exposure` should only contains numeric values"
  )
})

test_that("validation fails for non-factor group", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.character(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "`group` must be a factor variable."
  )
})

test_that("validation fails for incorrect MCMC parameters", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = -5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "df should be a non-negative integer."
  )

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 50,
      nburn = 100,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "`nits` has to be larger than `nburn`."
  )

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 60,
      chains = 1,
      family = "gaussian"
    ),
    "`nthin` cannot be larger or equal"
  )
})

test_that("validation fails for inconsistent rows between inputs", {
  d <- sbd_bdlim[1:50, ]
  d$id <- factor(1:50)
  d$y_binom <- rbinom(nrow(d), 1, 0.5)

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[1:49, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "Inconsistent number of rows between inputs found."
  )

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[1:49, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "Inconsistent number of rows between inputs found."
  )
})

test_that("validation fails for NA values", {
  d <- sbd_bdlim[1:50, ]
  d$id <- factor(1:50)
  d$y_binom <- rbinom(nrow(d), 1, 0.5)
  d$bwgaz[1] <- NA

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "NA found. Handling of NA is currently not supported."
  )
})

test_that("validation fails if group is included in covars", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI", "ChildSex"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "The same variable as the grouping variable is detected in `covars`."
  )
})

test_that("validation fails for incorrect id type or values", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = as.character(d$id), # Incorrect type
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "`id` must be a factor variable."
  )

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = factor(1:length(d$bwgaz)), # As many `id` as observations
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "There cannot be as many `id` as observations."
  )

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = factor(rep(1, length(d$bwgaz))), # Less than 2 levels
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian"
    ),
    "`id` must have at least 2 levels."
  )
})

test_that("validation loglik_all fails", {
  d <- sbd_bdlim[1:50, ]

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian",
      loglik_all = 1
    )
  )

  expect_error(
    validate_bdlim(
      y = d$bwgaz,
      exposure = as.matrix(d[, paste0("pm25_", 1:37)]),
      covars = d[, c("MomPriorBMI"), drop = FALSE],
      group = as.factor(d$ChildSex),
      id = NULL,
      df = 5,
      nits = 10,
      nburn = 5,
      nthin = 1,
      chains = 1,
      family = "gaussian",
      loglik_all = c(FALSE, TRUE)
    )
  )
})

test_that("loglik_all works", {
  d <- sbd_bdlim[1:50, ]

  f1 <- bdlim4(
    y = sbd_bdlim$bwgaz,
    exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
    covars = sbd_bdlim[, c("MomPriorBMI"), drop = FALSE],
    group = as.factor(sbd_bdlim$ChildSex),
    df = 5,
    nits = 10,
    model = "n",
    family = "gaussian",
    loglik_all = TRUE
  )

  expect_true(is.matrix(f1$fit_n$ll_all_keep))

  f2 <- bdlim4(
    y = sbd_bdlim$bwgaz,
    exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
    covars = sbd_bdlim[, c("MomPriorBMI"), drop = FALSE],
    group = as.factor(sbd_bdlim$ChildSex),
    df = 5,
    nits = 10,
    model = "n",
    family = "gaussian",
    loglik_all = FALSE
  )

  expect_true(is.null(f2$fit_n$ll_all_keep))
})

test_that("covars can be NULL", {
  d <- sbd_bdlim[1:50, ]

  expect_no_error(
    bdlim4(
      y = sbd_bdlim$bwgaz,
      exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
      covars = NULL,
      group = as.factor(sbd_bdlim$ChildSex),
      df = 5,
      nits = 10,
      model = "n",
      family = "gaussian",
      loglik_all = TRUE
    )
  )
})
