# load ####
load_all()
?bdlim4

set.seed(1234)
sbd_bdlim$id <- rbinom(nrow(sbd_bdlim), 3, 0.5) |> factor()
sbd_bdlim$y_binom <- rbinom(nrow(sbd_bdlim), 1, 0.5)
fit_sex <- bdlim4(
  y = sbd_bdlim$y_binom,
  exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
  covars = sbd_bdlim[, "MomPriorBMI", drop = F],
  group = as.factor(sbd_bdlim$race),
  id = sbd_bdlim$id,
  model = "w",
  df = 5,
  nits = 2,
  family = "binomial",
  cpp = TRUE
)

fit_sex$fit_n$model
sfit_sex
sfit_sex <- fit_sex |> summary()
sfit_sex |> plot()
sfit_sex$dlfun


# gaussian cpp ####
# b <- readRDS("calls.rds")
# nits <- 2
# set.seed(1234)
# f_r <- bdlim1_gaussian_partial(
#   y = b$y, nits = 1, design_input = b$design, nRE = b$nRE, REmodel = b$REmodel, w_group_ids = b$w_group_ids,
#   Edesign = b$Edesign, basis = b$basis, w_input = b$w, theta_input = b$theta, exposure = b$exposure
# )
# f_r$w_keep
# set.seed(1234)
# f_cpp <- bdlim1_gaussian_cpp(
#   y = b$y, nits = 1, design_input = b$design, nRE = b$nRE, REmodel = b$REmodel, w_group_ids = b$w_group_ids,
#   Edesign = b$Edesign, basis = b$basis, w_input = b$w, theta_input = b$theta, exposure = b$exposure
# )
# f_cpp$w_keep

# logistic cpp ####
# b <- readRDS("calls.rds")
# set.seed(1235)
# f_r <- bdlim1_logistic_partial(
#   y = b$y, nits = 1, design_input = b$design, nRE = b$nRE, REmodel = b$REmodel, w_group_ids = b$w_group_ids,
#   Edesign = b$Edesign, basis = b$basis, w_input = b$w, theta_input = b$theta, exposure = b$exposure
# )
#
# set.seed(1235)
# f_cpp <- bdlim1_logistic_cpp(
#   y = b$y, nits = 1, design_input = b$design, nRE = b$nRE, REmodel = b$REmodel, w_group_ids = b$w_group_ids,
#   Edesign = b$Edesign, basis = b$basis, w_input = b$w, theta_input = b$theta, exposure = b$exposure
# )

# Save the calls ####
# saveRDS(
#   list(
#     y = y,
#     w = w,
#     nits = nits,
#     design = design,
#     nRE = nRE,
#     REprec = REprec,
#     n_regcoef = n_regcoef,
#     REmodel = REmodel,
#     RElocation = RElocation,
#     n_weight_groups = n_weight_groups,
#     w_group_ids = w_group_ids,
#     theta = theta,
#     df = df,
#     basis = basis,
#     Edesign = Edesign,
#     exposure = exposure,
#     w_keep = w_keep,
#     regcoef_keep = regcoef_keep,
#     REprec_keep = REprec_keep,
#     ll_sum_keep = ll_sum_keep,
#     ll_all_keep = ll_all_keep,
#     names_groups = names_groups,
#     n_times = n_times,
#     b_free = b_free,
#     w_free = w_free,
#     n_groups = n_groups,
#     iter_keep = iter_keep
#   ),
#   "calls.rds"
# )

# Benchmark ####
# install()
# microbenchmark::microbenchmark(
#   bdlim4(
#     y = sbd_bdlim$bwgaz,
#     exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
#     covars = sbd_bdlim[, "MomPriorBMI", drop = F],
#     group = as.factor(sbd_bdlim$race),
#     # id = sbd_bdlim$id,
#     model = "n",
#     df = 5,
#     nits = 5000,
#     cpp = FALSE
#   ),
#   bdlim4(
#     y = sbd_bdlim$bwgaz,
#     exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
#     covars = sbd_bdlim[, "MomPriorBMI", drop = F],
#     group = as.factor(sbd_bdlim$race),
#     # id = sbd_bdlim$id,
#     model = "n",
#     df = 5,
#     nits = 5000,
#     cpp = TRUE
#   ),
#   times = 2
# )
