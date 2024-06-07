#' Simulated Birth Data
#'
#' A dataset containing simulated birth data for examples with bdlim. Add outcome and covariate data is simulated. The exposure data is real exposure data. Therefore, it has realistic correlation structure. The exposures are consistent with the date of conception variables. Each exposure is scaled by its IQR.
#'
#' @format A data frame with 1000 rows (observations) and 202 variables:
#' \describe{
#'   \item{bwgaz}{Outcome to be used. Simulated birth weight for gestational age z-score.}
#'   \item{ChildSex}{Binary sex of child.}
#'   \item{MomAge}{Continuous age in years.}
#'   \item{GestAge}{Continuous estimated gestational age at birth in weeks.}
#'   \item{MomHeightIn}{Continuous maternal height in inches.}
#'   \item{MomPriorWeightLbs}{Continuous mothers pre-pregnancy weight in pounds.}
#'   \item{MomPriorBMI}{Continuous mothers pre-pregnancy BMI.}
#'   \item{race}{Categorical race.}
#'   \item{Hispanic}{Binary indicator of Hispanic.}
#'   \item{MomEdu}{Categorical maternal heighest educational attainment.}
#'   \item{SmkAny}{Binary indicator of any smoking during pregnancy.}
#'   \item{Marital}{Categorical maternal marital status.}
#'   \item{Income}{Categorical income.}
#'   \item{EstDateConcept}{Estimated date of conception.}
#'   \item{EstMonthConcept}{Estimated month of conception.}
#'   \item{EstYearConcept}{Estimated year of conception.}
#'   \item{pm25_1}{Exposure to be used. Weekly average exposure to PM2.5 in week 1 of gestation.}
#'   \item{pm25_2}{Exposure to be used. Weekly average exposure to PM2.5 in week 2 of gestation.}
#'   \item{pm25_3}{Exposure to be used. Weekly average exposure to PM2.5 in week 3 of gestation.}
#'   \item{pm25_4}{Exposure to be used. Weekly average exposure to PM2.5 in week 4 of gestation.}
#'   \item{pm25_5}{Exposure to be used. Weekly average exposure to PM2.5 in week 5 of gestation.}
#'   \item{pm25_6}{Exposure to be used. Weekly average exposure to PM2.5 in week 6 of gestation.}
#'   \item{pm25_7}{Exposure to be used. Weekly average exposure to PM2.5 in week 7 of gestation.}
#'   \item{pm25_8}{Exposure to be used. Weekly average exposure to PM2.5 in week 8 of gestation.}
#'   \item{pm25_9}{Exposure to be used. Weekly average exposure to PM2.5 in week 9 of gestation.}
#'   \item{pm25_10}{Exposure to be used. Weekly average exposure to PM2.5 in week 10 of gestation.}
#'   \item{pm25_11}{Exposure to be used. Weekly average exposure to PM2.5 in week 11 of gestation.}
#'   \item{pm25_12}{Exposure to be used. Weekly average exposure to PM2.5 in week 12 of gestation.}
#'   \item{pm25_13}{Exposure to be used. Weekly average exposure to PM2.5 in week 13 of gestation.}
#'   \item{pm25_14}{Exposure to be used. Weekly average exposure to PM2.5 in week 14 of gestation.}
#'   \item{pm25_15}{Exposure to be used. Weekly average exposure to PM2.5 in week 15 of gestation.}
#'   \item{pm25_16}{Exposure to be used. Weekly average exposure to PM2.5 in week 16 of gestation.}
#'   \item{pm25_17}{Exposure to be used. Weekly average exposure to PM2.5 in week 17 of gestation.}
#'   \item{pm25_18}{Exposure to be used. Weekly average exposure to PM2.5 in week 18 of gestation.}
#'   \item{pm25_19}{Exposure to be used. Weekly average exposure to PM2.5 in week 19 of gestation.}
#'   \item{pm25_20}{Exposure to be used. Weekly average exposure to PM2.5 in week 20 of gestation.}
#'   \item{pm25_21}{Exposure to be used. Weekly average exposure to PM2.5 in week 21 of gestation.}
#'   \item{pm25_22}{Exposure to be used. Weekly average exposure to PM2.5 in week 22 of gestation.}
#'   \item{pm25_23}{Exposure to be used. Weekly average exposure to PM2.5 in week 23 of gestation.}
#'   \item{pm25_24}{Exposure to be used. Weekly average exposure to PM2.5 in week 24 of gestation.}
#'   \item{pm25_25}{Exposure to be used. Weekly average exposure to PM2.5 in week 25 of gestation.}
#'   \item{pm25_26}{Exposure to be used. Weekly average exposure to PM2.5 in week 26 of gestation.}
#'   \item{pm25_27}{Exposure to be used. Weekly average exposure to PM2.5 in week 27 of gestation.}
#'   \item{pm25_28}{Exposure to be used. Weekly average exposure to PM2.5 in week 28 of gestation.}
#'   \item{pm25_29}{Exposure to be used. Weekly average exposure to PM2.5 in week 29 of gestation.}
#'   \item{pm25_30}{Exposure to be used. Weekly average exposure to PM2.5 in week 30 of gestation.}
#'   \item{pm25_31}{Exposure to be used. Weekly average exposure to PM2.5 in week 31 of gestation.}
#'   \item{pm25_32}{Exposure to be used. Weekly average exposure to PM2.5 in week 32 of gestation.}
#'   \item{pm25_33}{Exposure to be used. Weekly average exposure to PM2.5 in week 33 of gestation.}
#'   \item{pm25_34}{Exposure to be used. Weekly average exposure to PM2.5 in week 34 of gestation.}
#'   \item{pm25_35}{Exposure to be used. Weekly average exposure to PM2.5 in week 35 of gestation.}
#'   \item{pm25_36}{Exposure to be used. Weekly average exposure to PM2.5 in week 36 of gestation.}
#'   \item{pm25_37}{Exposure to be used. Weekly average exposure to PM2.5 in week 37 of gestation.}
#'   \item{no2_1}{Exposure to be used. Weekly average exposure to NO2 in week 1 of gestation.}
#'   \item{no2_2}{Exposure to be used. Weekly average exposure to NO2 in week 2 of gestation.}
#'   \item{no2_3}{Exposure to be used. Weekly average exposure to NO2 in week 3 of gestation.}
#'   \item{no2_4}{Exposure to be used. Weekly average exposure to NO2 in week 4 of gestation.}
#'   \item{no2_5}{Exposure to be used. Weekly average exposure to NO2 in week 5 of gestation.}
#'   \item{no2_6}{Exposure to be used. Weekly average exposure to NO2 in week 6 of gestation.}
#'   \item{no2_7}{Exposure to be used. Weekly average exposure to NO2 in week 7 of gestation.}
#'   \item{no2_8}{Exposure to be used. Weekly average exposure to NO2 in week 8 of gestation.}
#'   \item{no2_9}{Exposure to be used. Weekly average exposure to NO2 in week 9 of gestation.}
#'   \item{no2_10}{Exposure to be used. Weekly average exposure to NO2 in week 10 of gestation.}
#'   \item{no2_11}{Exposure to be used. Weekly average exposure to NO2 in week 11 of gestation.}
#'   \item{no2_12}{Exposure to be used. Weekly average exposure to NO2 in week 12 of gestation.}
#'   \item{no2_13}{Exposure to be used. Weekly average exposure to NO2 in week 13 of gestation.}
#'   \item{no2_14}{Exposure to be used. Weekly average exposure to NO2 in week 14 of gestation.}
#'   \item{no2_15}{Exposure to be used. Weekly average exposure to NO2 in week 15 of gestation.}
#'   \item{no2_16}{Exposure to be used. Weekly average exposure to NO2 in week 16 of gestation.}
#'   \item{no2_17}{Exposure to be used. Weekly average exposure to NO2 in week 17 of gestation.}
#'   \item{no2_18}{Exposure to be used. Weekly average exposure to NO2 in week 18 of gestation.}
#'   \item{no2_19}{Exposure to be used. Weekly average exposure to NO2 in week 19 of gestation.}
#'   \item{no2_20}{Exposure to be used. Weekly average exposure to NO2 in week 20 of gestation.}
#'   \item{no2_21}{Exposure to be used. Weekly average exposure to NO2 in week 21 of gestation.}
#'   \item{no2_22}{Exposure to be used. Weekly average exposure to NO2 in week 22 of gestation.}
#'   \item{no2_23}{Exposure to be used. Weekly average exposure to NO2 in week 23 of gestation.}
#'   \item{no2_24}{Exposure to be used. Weekly average exposure to NO2 in week 24 of gestation.}
#'   \item{no2_25}{Exposure to be used. Weekly average exposure to NO2 in week 25 of gestation.}
#'   \item{no2_26}{Exposure to be used. Weekly average exposure to NO2 in week 26 of gestation.}
#'   \item{no2_27}{Exposure to be used. Weekly average exposure to NO2 in week 27 of gestation.}
#'   \item{no2_28}{Exposure to be used. Weekly average exposure to NO2 in week 28 of gestation.}
#'   \item{no2_29}{Exposure to be used. Weekly average exposure to NO2 in week 29 of gestation.}
#'   \item{no2_30}{Exposure to be used. Weekly average exposure to NO2 in week 30 of gestation.}
#'   \item{no2_31}{Exposure to be used. Weekly average exposure to NO2 in week 31 of gestation.}
#'   \item{no2_32}{Exposure to be used. Weekly average exposure to NO2 in week 32 of gestation.}
#'   \item{no2_33}{Exposure to be used. Weekly average exposure to NO2 in week 33 of gestation.}
#'   \item{no2_34}{Exposure to be used. Weekly average exposure to NO2 in week 34 of gestation.}
#'   \item{no2_35}{Exposure to be used. Weekly average exposure to NO2 in week 35 of gestation.}
#'   \item{no2_36}{Exposure to be used. Weekly average exposure to NO2 in week 36 of gestation.}
#'   \item{no2_37}{Exposure to be used. Weekly average exposure to NO2 in week 37 of gestation.}
#'   \item{so2_1}{Exposure to be used. Weekly average exposure to SO2 in week 1 of gestation.}
#'   \item{so2_2}{Exposure to be used. Weekly average exposure to SO2 in week 2 of gestation.}
#'   \item{so2_3}{Exposure to be used. Weekly average exposure to SO2 in week 3 of gestation.}
#'   \item{so2_4}{Exposure to be used. Weekly average exposure to SO2 in week 4 of gestation.}
#'   \item{so2_5}{Exposure to be used. Weekly average exposure to SO2 in week 5 of gestation.}
#'   \item{so2_6}{Exposure to be used. Weekly average exposure to SO2 in week 6 of gestation.}
#'   \item{so2_7}{Exposure to be used. Weekly average exposure to SO2 in week 7 of gestation.}
#'   \item{so2_8}{Exposure to be used. Weekly average exposure to SO2 in week 8 of gestation.}
#'   \item{so2_9}{Exposure to be used. Weekly average exposure to SO2 in week 9 of gestation.}
#'   \item{so2_10}{Exposure to be used. Weekly average exposure to SO2 in week 10 of gestation.}
#'   \item{so2_11}{Exposure to be used. Weekly average exposure to SO2 in week 11 of gestation.}
#'   \item{so2_12}{Exposure to be used. Weekly average exposure to SO2 in week 12 of gestation.}
#'   \item{so2_13}{Exposure to be used. Weekly average exposure to SO2 in week 13 of gestation.}
#'   \item{so2_14}{Exposure to be used. Weekly average exposure to SO2 in week 14 of gestation.}
#'   \item{so2_15}{Exposure to be used. Weekly average exposure to SO2 in week 15 of gestation.}
#'   \item{so2_16}{Exposure to be used. Weekly average exposure to SO2 in week 16 of gestation.}
#'   \item{so2_17}{Exposure to be used. Weekly average exposure to SO2 in week 17 of gestation.}
#'   \item{so2_18}{Exposure to be used. Weekly average exposure to SO2 in week 18 of gestation.}
#'   \item{so2_19}{Exposure to be used. Weekly average exposure to SO2 in week 19 of gestation.}
#'   \item{so2_20}{Exposure to be used. Weekly average exposure to SO2 in week 20 of gestation.}
#'   \item{so2_21}{Exposure to be used. Weekly average exposure to SO2 in week 21 of gestation.}
#'   \item{so2_22}{Exposure to be used. Weekly average exposure to SO2 in week 22 of gestation.}
#'   \item{so2_23}{Exposure to be used. Weekly average exposure to SO2 in week 23 of gestation.}
#'   \item{so2_24}{Exposure to be used. Weekly average exposure to SO2 in week 24 of gestation.}
#'   \item{so2_25}{Exposure to be used. Weekly average exposure to SO2 in week 25 of gestation.}
#'   \item{so2_26}{Exposure to be used. Weekly average exposure to SO2 in week 26 of gestation.}
#'   \item{so2_27}{Exposure to be used. Weekly average exposure to SO2 in week 27 of gestation.}
#'   \item{so2_28}{Exposure to be used. Weekly average exposure to SO2 in week 28 of gestation.}
#'   \item{so2_29}{Exposure to be used. Weekly average exposure to SO2 in week 29 of gestation.}
#'   \item{so2_30}{Exposure to be used. Weekly average exposure to SO2 in week 30 of gestation.}
#'   \item{so2_31}{Exposure to be used. Weekly average exposure to SO2 in week 31 of gestation.}
#'   \item{so2_32}{Exposure to be used. Weekly average exposure to SO2 in week 32 of gestation.}
#'   \item{so2_33}{Exposure to be used. Weekly average exposure to SO2 in week 33 of gestation.}
#'   \item{so2_34}{Exposure to be used. Weekly average exposure to SO2 in week 34 of gestation.}
#'   \item{so2_35}{Exposure to be used. Weekly average exposure to SO2 in week 35 of gestation.}
#'   \item{so2_36}{Exposure to be used. Weekly average exposure to SO2 in week 36 of gestation.}
#'   \item{so2_37}{Exposure to be used. Weekly average exposure to SO2 in week 37 of gestation.}
#'   \item{co_1}{Exposure to be used. Weekly average exposure to CO in week 1 of gestation.}
#'   \item{co_2}{Exposure to be used. Weekly average exposure to CO in week 2 of gestation.}
#'   \item{co_3}{Exposure to be used. Weekly average exposure to CO in week 3 of gestation.}
#'   \item{co_4}{Exposure to be used. Weekly average exposure to CO in week 4 of gestation.}
#'   \item{co_5}{Exposure to be used. Weekly average exposure to CO in week 5 of gestation.}
#'   \item{co_6}{Exposure to be used. Weekly average exposure to CO in week 6 of gestation.}
#'   \item{co_7}{Exposure to be used. Weekly average exposure to CO in week 7 of gestation.}
#'   \item{co_8}{Exposure to be used. Weekly average exposure to CO in week 8 of gestation.}
#'   \item{co_9}{Exposure to be used. Weekly average exposure to CO in week 9 of gestation.}
#'   \item{co_10}{Exposure to be used. Weekly average exposure to CO in week 10 of gestation.}
#'   \item{co_11}{Exposure to be used. Weekly average exposure to CO in week 11 of gestation.}
#'   \item{co_12}{Exposure to be used. Weekly average exposure to CO in week 12 of gestation.}
#'   \item{co_13}{Exposure to be used. Weekly average exposure to CO in week 13 of gestation.}
#'   \item{co_14}{Exposure to be used. Weekly average exposure to CO in week 14 of gestation.}
#'   \item{co_15}{Exposure to be used. Weekly average exposure to CO in week 15 of gestation.}
#'   \item{co_16}{Exposure to be used. Weekly average exposure to CO in week 16 of gestation.}
#'   \item{co_17}{Exposure to be used. Weekly average exposure to CO in week 17 of gestation.}
#'   \item{co_18}{Exposure to be used. Weekly average exposure to CO in week 18 of gestation.}
#'   \item{co_19}{Exposure to be used. Weekly average exposure to CO in week 19 of gestation.}
#'   \item{co_20}{Exposure to be used. Weekly average exposure to CO in week 20 of gestation.}
#'   \item{co_21}{Exposure to be used. Weekly average exposure to CO in week 21 of gestation.}
#'   \item{co_22}{Exposure to be used. Weekly average exposure to CO in week 22 of gestation.}
#'   \item{co_23}{Exposure to be used. Weekly average exposure to CO in week 23 of gestation.}
#'   \item{co_24}{Exposure to be used. Weekly average exposure to CO in week 24 of gestation.}
#'   \item{co_25}{Exposure to be used. Weekly average exposure to CO in week 25 of gestation.}
#'   \item{co_26}{Exposure to be used. Weekly average exposure to CO in week 26 of gestation.}
#'   \item{co_27}{Exposure to be used. Weekly average exposure to CO in week 27 of gestation.}
#'   \item{co_28}{Exposure to be used. Weekly average exposure to CO in week 28 of gestation.}
#'   \item{co_29}{Exposure to be used. Weekly average exposure to CO in week 29 of gestation.}
#'   \item{co_30}{Exposure to be used. Weekly average exposure to CO in week 30 of gestation.}
#'   \item{co_31}{Exposure to be used. Weekly average exposure to CO in week 31 of gestation.}
#'   \item{co_32}{Exposure to be used. Weekly average exposure to CO in week 32 of gestation.}
#'   \item{co_33}{Exposure to be used. Weekly average exposure to CO in week 33 of gestation.}
#'   \item{co_34}{Exposure to be used. Weekly average exposure to CO in week 34 of gestation.}
#'   \item{co_35}{Exposure to be used. Weekly average exposure to CO in week 35 of gestation.}
#'   \item{co_36}{Exposure to be used. Weekly average exposure to CO in week 36 of gestation.}
#'   \item{co_37}{Exposure to be used. Weekly average exposure to CO in week 37 of gestation.}
#'   \item{temp_1}{Exposure to be used. Weekly average exposure to temperature in week 1 of gestation.}
#'   \item{temp_2}{Exposure to be used. Weekly average exposure to temperature in week 2 of gestation.}
#'   \item{temp_3}{Exposure to be used. Weekly average exposure to temperature in week 3 of gestation.}
#'   \item{temp_4}{Exposure to be used. Weekly average exposure to temperature in week 4 of gestation.}
#'   \item{temp_5}{Exposure to be used. Weekly average exposure to temperature in week 5 of gestation.}
#'   \item{temp_6}{Exposure to be used. Weekly average exposure to temperature in week 6 of gestation.}
#'   \item{temp_7}{Exposure to be used. Weekly average exposure to temperature in week 7 of gestation.}
#'   \item{temp_8}{Exposure to be used. Weekly average exposure to temperature in week 8 of gestation.}
#'   \item{temp_9}{Exposure to be used. Weekly average exposure to temperature in week 9 of gestation.}
#'   \item{temp_10}{Exposure to be used. Weekly average exposure to temperature in week 10 of gestation.}
#'   \item{temp_11}{Exposure to be used. Weekly average exposure to temperature in week 11 of gestation.}
#'   \item{temp_12}{Exposure to be used. Weekly average exposure to temperature in week 12 of gestation.}
#'   \item{temp_13}{Exposure to be used. Weekly average exposure to temperature in week 13 of gestation.}
#'   \item{temp_14}{Exposure to be used. Weekly average exposure to temperature in week 14 of gestation.}
#'   \item{temp_15}{Exposure to be used. Weekly average exposure to temperature in week 15 of gestation.}
#'   \item{temp_16}{Exposure to be used. Weekly average exposure to temperature in week 16 of gestation.}
#'   \item{temp_17}{Exposure to be used. Weekly average exposure to temperature in week 17 of gestation.}
#'   \item{temp_18}{Exposure to be used. Weekly average exposure to temperature in week 18 of gestation.}
#'   \item{temp_19}{Exposure to be used. Weekly average exposure to temperature in week 19 of gestation.}
#'   \item{temp_20}{Exposure to be used. Weekly average exposure to temperature in week 20 of gestation.}
#'   \item{temp_21}{Exposure to be used. Weekly average exposure to temperature in week 21 of gestation.}
#'   \item{temp_22}{Exposure to be used. Weekly average exposure to temperature in week 22 of gestation.}
#'   \item{temp_23}{Exposure to be used. Weekly average exposure to temperature in week 23 of gestation.}
#'   \item{temp_24}{Exposure to be used. Weekly average exposure to temperature in week 24 of gestation.}
#'   \item{temp_25}{Exposure to be used. Weekly average exposure to temperature in week 25 of gestation.}
#'   \item{temp_26}{Exposure to be used. Weekly average exposure to temperature in week 26 of gestation.}
#'   \item{temp_27}{Exposure to be used. Weekly average exposure to temperature in week 27 of gestation.}
#'   \item{temp_28}{Exposure to be used. Weekly average exposure to temperature in week 28 of gestation.}
#'   \item{temp_29}{Exposure to be used. Weekly average exposure to temperature in week 29 of gestation.}
#'   \item{temp_30}{Exposure to be used. Weekly average exposure to temperature in week 30 of gestation.}
#'   \item{temp_31}{Exposure to be used. Weekly average exposure to temperature in week 31 of gestation.}
#'   \item{temp_32}{Exposure to be used. Weekly average exposure to temperature in week 32 of gestation.}
#'   \item{temp_33}{Exposure to be used. Weekly average exposure to temperature in week 33 of gestation.}
#'   \item{temp_34}{Exposure to be used. Weekly average exposure to temperature in week 34 of gestation.}
#'   \item{temp_35}{Exposure to be used. Weekly average exposure to temperature in week 35 of gestation.}
#'   \item{temp_36}{Exposure to be used. Weekly average exposure to temperature in week 36 of gestation.}
#'   \item{temp_37}{Exposure to be used. Weekly average exposure to temperature in week 37 of gestation.}
#'   \item{source}{Variable indicating that the data came from the bdlim package.}
#' }
"sbd_bdlim"
