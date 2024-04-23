

# run BDLIM with modification by ChildSex
fit_sex <- bdlim4(
  y = simulated_birth_data_c1$bwgaz,
  exposure = simulated_birth_data_c1[,paste0("pm25_",1:37)],
  covars = simulated_birth_data_c1[,c("MomPriorBMI","MomAge","race","Hispanic",
                                      "EstMonthConcept","EstYearConcept")],
  group = as.factor(simulated_birth_data_c1$ChildSex),
  df = 5,
  nits = 5000,
  parallel = FALSE
)

# show model comparison results
fit_sex

#summarize results
sfit_sex <- summary(fit_sex)

# graph the estimated distributed lag functions for each group
plot(sfit_sex)

# can save plot as an object and modify with ggplot2
library(ggplot2)
plt <- plot(sfit_sex)
plt + ggtitle("My plot with BDLIM") +
 ylab("Estimated expected difference in\nBWGAZ per 1 ug/m3 increase in exposure")

# the summary file has the data to make this plot
head(sfit_sex$dlfun)
