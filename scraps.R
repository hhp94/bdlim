# load ####
load_all()
?bdlim4

set.seed(1234)
sbd_bdlim$id <- rbinom(nrow(sbd_bdlim), 3, 0.5) |> factor()
fit_sex <- bdlim4(
  y = sbd_bdlim$bwgaz,
  exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
  covars = sbd_bdlim[, "MomPriorBMI", drop = F],
  group = as.factor(sbd_bdlim$race),
  id = sbd_bdlim$id,
  model = "b",
  df = 5,
  nits = 2,
  cpp = FALSE
)

sfit_sex <- fit_sex |> summary()
sfit_sex$dlfun
sfit_sex |> plot()

# gaussian cpp ####
b <- readRDS("calls.rds")
nits <- 2
set.seed(1234)
f_r <- bdlim1_gaussian_partial(
  y = b$y, nits = nits, design = b$design, nRE = b$nRE, REmodel = FALSE, w_group_ids = b$w_group_ids,
  basis = b$basis, theta = b$theta, Edesign = b$Edesign, exposure = b$exposure, w = b$w, w_keep = b$w_keep, b$regcoef_keep,
  ll_sum_keep = b$ll_sum_keep, ll_all_keep = b$ll_all_keep, iter_keep = b$iter_keep
)
f_r
set.seed(1234)
f_cpp <- bdlim1_gaussian_cpp(
  y = b$y, nits = nits, design = b$design, nRE = b$nRE, REmodel = FALSE, w_group_ids = b$w_group_ids,
  basis = b$basis, theta = b$theta, Edesign = b$Edesign, exposure = b$exposure, w = b$w
)
f_cpp

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
