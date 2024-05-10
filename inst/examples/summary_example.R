\donttest{

# run BDLIM with modification by ChildSex
fit_sex <- bdlim4(
  y = sbd_bdlim$bwgaz,
  exposure = sbd_bdlim[,paste0("pm25_",1:37)],
  covars = sbd_bdlim[,c("MomPriorBMI","MomAge","race","Hispanic",
                                      "EstMonthConcept","EstYearConcept")],
  group = as.factor(sbd_bdlim$ChildSex),
  df = 5,
  nits = 5000,
  parallel = FALSE
)

#summarize results
summary(fit_sex)

# obtain estimates of the distributed lag function
# these are note displayed when printed but available for use
sfit_sex <- summary(fit_sex)
head(sfit_sex$dlfun)

# can summarize with a specific model
sfit_hisp_n <- summary(fit_sex, model="n") # no modification
sfit_hisp_b <- summary(fit_sex, model="b") # subgroup-specific effects (beta)
sfit_hisp_w <- summary(fit_sex, model="w") # subgroup-specific weight function
sfit_hisp_bw <- summary(fit_sex, model="bw") # both subgroup-specific

}
