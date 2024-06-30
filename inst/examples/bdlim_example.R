\donttest{
library(bdlim)
library(future)

# Change MCMC convergence threshold to suppress warnings.
options(bdlim_rhat_thresh_u = 1.01)
options(bdlim_ess_thresh = 400)

# To run in parallel, change workers to > 1
plan(multisession, workers = 1)
# run BDLIM with modification by ChildSex
fit_sex <- bdlim4(
  y = sbd_bdlim$bwgaz,
  exposure = sbd_bdlim[, paste0("pm25_", 1:37)],
  covars = sbd_bdlim[
    , c("MomPriorBMI", "MomAge", "race", "Hispanic", "EstMonthConcept", "EstYearConcept")
  ],
  group = as.factor(sbd_bdlim$ChildSex),
  df = 5,
  nits = 100 # Set to 100 for example, increase in analysis
)
plan(sequential)

# show model comparison results
fit_sex

# summarize results
sfit_sex <- summary(fit_sex)
sfit_sex

# graph the estimated distributed lag functions for each group
plot(sfit_sex)
}
