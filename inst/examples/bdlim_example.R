\donttest{
library(bdlim)
library(future)

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
  nits = 1000 # Set to 100 for example, increase in analysis
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
