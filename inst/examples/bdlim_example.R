

# run BDLIM with modification by ChildSex
fit_sex <- bdlim4(
  y = sbd_bdlim$bwgaz,
  exposure = sbd_bdlim[,paste0("pm25_",1:37)],
  covars = sbd_bdlim[,c("MomPriorBMI","MomAge","race","Hispanic",
                                   "EstMonthConcept","EstYearConcept")],
  group = as.factor(sbd_bdlim$ChildSex),
  id = as.factor(rep(1:100,10)),
  df = 5,
  nits = 5000,
  parallel = FALSE
)

# show model comparison results
fit_sex

#summarize results
sfit_sex <- summary(fit_sex)
sfit_sex
# graph the estimated distributed lag functions for each group
plot(sfit_sex)



# run BDLIM with modification by Hispanic
fit_hisp <- bdlim4(
  y = sbd_bdlim$bwgaz,
  exposure = sbd_bdlim[,paste0("pm25_",1:37)],
  covars = sbd_bdlim[,c("MomPriorBMI","MomAge","race","ChildSex",
                                      "EstMonthConcept","EstYearConcept")],
  group = as.factor(sbd_bdlim$Hispanic),
  df = 5,
  nits = 5000,
  parallel = FALSE
)

# show model comparison results
fit_hisp

# summarize results for best fitting pattern of modification
sfit_hisp <- summary(fit_hisp)

# graph the estimated distributed lag functions for each group
plot(sfit_hisp)

# summarize and force results from model with
# subgroup-specific effects and weights
sfit_hisp_bw <- summary(fit_hisp, model="bw")
plot(sfit_hisp_bw)

